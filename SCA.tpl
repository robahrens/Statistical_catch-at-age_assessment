//><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>
//Programer: Robert Ahrens													 
//Purpose:SCA simulator-assessment model.											 
//Notes: 				 
//							 
//																 
//><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>

DATA_SECTION
	int sim;
	int rseed;
	
	LOCAL_CALCS
		sim=0;
		rseed=0;
		int on,opt;
		if((on=option_match(ad_comm::argc,ad_comm::argv,"-sim",opt))>-1)
		{
			sim=1;
			rseed=atoi(ad_comm::argv[on+1]);
		}
	END_CALCS

	init_adstring datafile;
	init_adstring ctlfile;
	//change to the new data file
	!!ad_comm::change_datafile_name(datafile);
	//for mcmc stuff
	init_int syr;
	init_int eyr;
	init_int nage;
	init_number alpha;
	init_number rho;
	init_number wla;
	init_number wlb;
	init_vector yt(syr,eyr);
	init_vector ct(syr,eyr);
	init_matrix pat(syr,eyr,1,nage);
	//!!cout<<pat<<endl;
	//!!ad_exit(1);
	init_number iro;
	init_number icr;
	init_number irbar;
	init_number ifbar;
	init_number iahat;
	init_number ighat;
	init_int eof;
	int iter;
	!!iter=0;

	LOCAL_CALCS
		if(eof!=999)
		{
			cout<<"Error reading data.\n Fix it."<<endl;
			ad_exit(1);
		}
	END_CALCS
	vector age(1,nage);
	vector la(1,nage);
	vector wa(1,nage);
	vector fa(1,nage);
	number m;
	LOCAL_CALCS
		m=1.2*vbk;
		age.fill_seqadd(1,1);
		la[1]=alpha;
		for (int i=2;i<=nage)la[i]=alpha+rho*la[i-1];
		wa=wla*pow(la,wlb);
		//weight at age * maturity at age
		fa=elem_prod(wa,plogis(age,log(3)/vbk,0.1*log(3)/vbk));
	END_CALCS
	//load in data for simulations
	!!ad_comm::change_datafile_name(ctlfile);
	init_number simsig;
	init_number simtau;
	init_number simqo;
	init_number simao;
	init_number simro;
	init_number simcr;
	init_number simrbar;
	init_number simahat;
	init_number simghat;
	init_vector simF(syr,eyr);
	init_int eofc;
	LOCAL_CALCS
		if(eofc!=999)
		{
			cout<<"Error reading control file.\n Fix it."<<endl;
			ad_exit(1);
		}
	END_CALCS

PARAMETER_SECTION
	init_number log_ro(4);
	init_number log_cr(5);
	init_number log_rbar;
	init_number log_fbar;
	init_bounded_number ahat(0,nage);
	init_bounded_number ghat(0,5);
	init_number ptdev;
	init_bounded_number rho(0,1);
	init_bounded_dev_vector wt(syr-nage,eyr,-10.,10.,2);
	init_bounded_dev_vector log_ft_dev(syr,eyr,-10.,10.,3);
	objective_function_value nll;
	!!log_ro=log(iro);
	!!log_cr=log(icr);
	!!log_rbar=log(irbar);
	!!log_fbar=log(ifbar);
	!!rho=0.5;
	!!ptdev=log(1./0.08);
	!!ahat=iahat;
	!!ghat=ighat;
	number ro;
	number cr;
	number rbar;
	number fbar;
	number so;
	number beta;
	number q;
	number fmsy;
	number msy;
	number bmsy;
	number sig;
	number tau;
	sdreport_number tdev;

	vector va(1,nage);
	vector ft(syr,eyr);
	vector bt(syr,eyr+1);
	vector ct_hat(syr,eyr);
	vector ct_resid(syr,eyr);
	vector yt_resid(syr,eyr);
	vector rt(syr+1,eyr);
	vector rt_resid(syr+1,eyr);

	matrix Nt(syr,eyr+1,1,nage);
	matrix Zt(syr,eyr,1,nage);
	matrix pahat(syr,eyr,1,nage);

PRELIMINARY_CALCS_SECTION
  if(sim)
  {
  	run_data_simulation();
  }

PROCEDURE_SECTION
	initialization();
	statedynamics();
	observation_model();
	stock_recruit_model();
	objective_function();

	if(mceval_phase())
	{ 
		forecast();
		get_CF(value(fmsy),value(msy),value(bmsy));
		mcmc_output();
	}
	if(last_phase())
	{
		forecast();
		get_CF(value(fmsy),value(msy),value(bmsy));
	} 

FUNCTION initialization
	va=plogis(age,ahat,ghat);
	ft=mfexp(log_fbar+log_ft_dev);
	Zt=m+outer_prod(ft,va);

FUNCTION statedynamics
	dvar_vector lxo=pow(mfexp(-m),age-1.);
	lxo(nage)/=(1.-mfexp(-m));
	Nt(syr,1)=mfexp(log_rbar+wt(syr-1));
	for(int j=2;j<=nage;j++) Nt(syr,j)=mfexp(log_rbar+wt(syr-j))*lxo(j);
	for(int i=syr;i<=eyr;i++)
	{
		Nt(i+1,1)=mfexp(log_rbar+wt(i));
		Nt(i+1)(2,nage)=++elem_prod(Nt(i)(1,nage-1),mfexp(-Zt(i)(1,nage-1)));
		Nt(i+1,nage)+=Nt(i,nage)*mfexp(-Zt(i,nage));
	}
	bt=Nt*elem_prod(wa,va);

FUNCTION observation_model
	dvar_matrix C(syr,eyr,1,nage);
	dvar_matrix F(syr,eyr,1,nage);
	F=outer_prod(ft,va);
	C=elem_prod(elem_div(F,Zt),elem_prod(1.-mfexp(-Zt),Nt));
	ct_hat=C*wa;
	//catch residuals
	ct_resid=log(ct)-log(ct_hat);
	//cpue residuals ala waters and ludwig 1994
	yt_resid=log(yt)-log(bt(syr,eyr));
	q=mfexp(mean(yt_resid));
	yt_resid-=mean(yt_resid);
	//predicted proportions in the catch
	for(int i=syr;i<=eyr;i++)pahat(i)=C(i)/sum(C(i));

FUNCTION stock_recruit_model
	ro=mfexp(log_ro);
	cr=mfexp(log_cr)+1.;
	dvar_vector lxo=pow(mfexp(-m),age-1.);
	lxo(nage)/=(1.-mfexp(-m));
	dvariable phieo=lxo*fa;
	so=cr/phieo;
	beta=(cr-1.)/(ro*phieo);
	dvar_vector sbt=Nt*fa;
	dvar_vector nmr=so*sbt(syr,eyr-1);
	dvar_vector den=(1.+beta*sbt(syr,eyr-1));
	dvar_vector tmp_rt=++elem_div(nmr,den);
	rt=column(Nt.sub(syr+1,eyr),1);
	rt_resid=log(tmp_rt)-log(rt);

FUNCTION objective_function 
	dvar_vector nll_vec(1,4);
	tdev=sqrt(1./mfexp(ptdev));
	sig=sqrt(rho*1./mfexp(ptdev));//process error sd.dev
	tau=sqrt((1.-rho)*1./mfexp(ptdev));
	nll_vec.initialize();
	nll_vec(1)=dnorm(ct_resid,0.05);
	nll_vec(2)=dnorm(yt_resid,tau);
	nll_vec(3)=dnorm(rt_resid,sig);
	double tau2;
	nll_vec(4)=dmvlogistic(pat,pahat,tau2);
	dvar_vector p_vec(1,5);
	p_vec.initialize();
	dvariable h=cr/(4.+cr);
	p_vec(1)=dbeta((h-0.2)/0.8,2,2);
	
	if(last_phase())
	{
		p_vec(3)=dnorm(wt,2);
		p_vec(4)=dnorm(log_ft_dev,2);
	}
	else
	{
		p_vec(3)=100.*norm2(wt);
		p_vec(4)=100.*norm2(log_ft_dev);

	}
	p_vec(5)=dbeta(rho,50,50);
	nll=sum(nll_vec)+sum(p_vec);

FUNCTION mcmc_output
	if(iter==0)
	{
		ofstream ofs("refpar.mcmc");
		ofs<<"fmsy\t bmsy\t msy\t b/bmsy\t f/fmsy"<<endl;
		//ofs<<"r\t k\t q\t sig\t"<<endl;
	}
	iter++;
	double fratio=value(ft(eyr)/fmsy);
	double bratio=value(Nt(eyr)*wa/bmsy);
	ofstream ofs("refpar.mcmc",ios::app);
	ofs<<fmsy<<"\t"<<bmsy<<"\t"<<msy<<"\t"<<bratio<<"\t"<<fratio<<endl;

FUNCTION forecast

FUNCTION run_data_simulation
	random_number_generator rng(rseed);
	dmatrix C(syr,eyr,1,nage);
	dvector tmp(syr-nage,eyr);
	dvector eps(syr,eyr);
	dvector simqt(syr,eyr);
	tmp.fill_randn(rng);
	eps.fill_randn(rng);
	wt=tmp*simsig;
	eps*=simtau;
	log_ro=log(simro);
	log_cr=log(simcr);
	log_rbar=log(simrbar);
	ahat=simahat;
	ghat=simghat;
	ro=mfexp(log_ro);
	cr=mfexp(log_cr);
	rbar=mfexp(log_rbar);
	dvector lxo=pow(mfexp(-m),age-1.);
	lxo(nage)/=(1.-mfexp(-m));
	double phieo=lxo*fa;
	double phibo=lxo*wa;
	double Bo=value(ro)*phibo;
	so=cr/phieo;
	beta=(cr-1.)/(ro*phieo);
	//selectivity
	va=plogis(age,ahat,ghat);
	//Make some fish
	//initial numbers
	Nt(syr,1)=mfexp(log_rbar+wt(syr-1));
	for(int j=2;j<=nage;j++)Nt(syr,j)=mfexp(log_rbar+wt(syr-j))*lxo(j);
	ft=simF;
	for(int i=syr;i<=eyr;i++)
	{
		dvector ba=value(elem_prod(Nt(i),wa));
		Zt(i)=m+ft(i)*va;
		//update numbers
		double sbt=value(Nt(i)*fa);
		simqt(i)=simqo/(simao+(1.-simao)*sum(ba)/Bo);
		Nt(i+1,1)=so*sbt/(1.+beta*sbt)*mfexp(wt(i));
		Nt(i+1)(2,nage)=++elem_prod(Nt(i)(1,nage-1),mfexp(-Zt(i)(1,nage-1)));
		Nt(i+1,nage)+=Nt(i,nage)*mfexp(-Zt(i,nage));
		dvector zttmp=value(Zt(i));
		C(i)=elem_prod(elem_div(value(ft(i)*va),zttmp),elem_prod(1.-mfexp(-zttmp),value(Nt(i))));
		pat(i)=rmvlogistic(C(i)(1,nage),0.3,rseed+i);

	}
	ct=C*wa;
	dvar_vector wbar(syr,eyr);
	for (int i=syr;i<=eyr;i++) wbar(i)=ct(i)/sum(C(i));
	bt=Nt*elem_prod(wa,va);
	yt=elem_prod(simqt,value(elem_prod(bt(syr,eyr),mfexp(eps))));
	wt=0;
	//use this code to create data files for more basic methods.
	/*
	ofstream ofs("DD.dat");
	ofs<<"#syr"<<endl;
	ofs<<syr<<endl;
	ofs<<"#eyr"<<endl;
	ofs<<eyr<<endl;
	ofs<<"#surv"<<endl;
	ofs<<mfexp(-m)<<endl;
	ofs<<"#rho"<<endl;
	ofs<<0.73<<endl;
	ofs<<"#alpha"<<endl;
	ofs<<11<<endl;
	ofs<<"#agek"<<endl;
	ofs<<3<<endl;
	ofs<<"#nage"<<endl;	
	ofs<<nage<<endl;
	ofs<<"#wa"<<endl;
	ofs<<wa<<endl;
	ofs<<"#yt"<<endl;
	ofs<<yt<<endl;
	ofs<<"#ct"<<endl;
	ofs<<ct<<endl;
	ofs<<"#wbar"<<endl;
	ofs<<wbar<<endl;
	ofs<<"#eof"<<endl;
	ofs<<999<<endl;
	*/
	//cout<<yt<<endl;
	//cout<<""<<endl;
	//cout<<ct<<endl;
	//cout<<""<<endl;
	//cout<<pat<<endl;
	//ad_exit(1);



FUNCTION void calc_partials(const double& fe, double& phie, double& phif, double& phiq, double& dphif_df, double& dphiq_df, double& dRe_df)
	//Use this function to calculate the partial derivatives (Table 2 in Martell et al. 2008)
	//Arguments: fe=fishing rate
	int i;
	dvector lx=(pow(exp(-m),age-1.));
	lx(nage)/=(1.-exp(-(m)));
	dvector lz(1,nage);
	dvector za=value(m+fe*va);
	dvector sa=1.-exp(-za);
	dvector qa=elem_prod(elem_div(value(va),za),sa);
	double dlz_df=0;

	lz[1]=1.0; 
	dphiq_df=0; dphif_df=0;
	phie=(sum(elem_prod(lx,fa)));
	for(i=1; i<=nage; i++)
	{
		if(i>1) lz[i]=lz[i-1]*exp(-za[i-1]);
		if(i>1) dlz_df=dlz_df*exp(-za[i-1]) - lz[i-1]*value(va[i-1])*exp(-za[i-1]);
		if(i==nage){ //6/11/2007 added plus group.
			lz[i]/=(1.-mfexp(-za[i]));
			//dlz_df=dlz_df*mfexp(-za[i-1]) - lz[i-1]*va[i-1]*mfexp(-za[i-1])/(1.-mfexp(-za[i]))
			dlz_df=value(dlz_df/(1.-mfexp(-za[i]))
					-lz[i-1]*mfexp(-za[i-1])*va[i]*mfexp(-za[i])
			/((1.-mfexp(-za[i]))*(1.-mfexp(-za[i]))));
		}	
		dphif_df=dphif_df+(fa[i])*dlz_df;
		dphiq_df=dphiq_df+(wa[i]*qa[i]*dlz_df+(lz[i]*wa[i]*value(va[i]*va[i]))/za[i]*(exp(-za[i])-sa[i]/za[i]));
	}
	phif=sum(elem_prod(lz,(fa)));
	phiq=sum(elem_prod(elem_prod(lz,(wa)),qa));
	dRe_df=value(ro/(cr-1.))*phie/square(phif)*dphif_df;
	//dphif_df=sum(elem_prod(fa,dlz_df));
	//dvector t2=elem_div(elem_prod(elem_prod(lz,value(va)),wa),za);
	//dvector t3=exp(-za)-elem_div(sa,za);
	//dphiq_df=sum(elem_prod(elem_prod(wa,qa),dlz_df)+elem_prod(t2,t3));



FUNCTION void get_CF(double& fe, double& msy,double& bmsy)
	//This function uses Newton-Raphson method to iteratively solve for F*
	//Then calculates C* given F* (See eq 1.3 in Martell 2008 CJFAS)
	int iter;
	double dy,ddy,re;
	double phie,phif,phiq,dphif_df,dphiq_df,dRe_df;
	fe=(m);  //initial guess for Fmsy
	for(iter= 1; iter<=50; iter++)
	{
		calc_partials(fe,phie,phif,phiq,dphif_df,dphiq_df,dRe_df);
		re=value(ro*(cr-phie/phif)/(cr-1.));
		dy=re*phiq+fe*phiq*dRe_df+fe*re*dphiq_df;
		ddy=phiq*dRe_df+re*dphiq_df;
		//Newton update
		fe=fe-dy/ddy;
		if(sfabs(dy)<1.e-10)break;
		//cout<<"Fe dy\t"<<fe<<" "<<dy<<" "<<fe-dy/ddy<<endl;
	}
	msy=fe*re*phiq;
	bmsy=re*phif;
	//cout<<"Fe "<<fe<<endl;



REPORT_SECTION
	
	//double fmsy;
	//double msy;
	//double bmsy;
	//if(last_phase())
	//{
	//	get_CF(fmsy,msy,bmsy);
	//}
	REPORT(ro);
	REPORT(cr);
	REPORT(mfexp(log_rbar));
	REPORT(mfexp(log_rbar+wt));
	REPORT(mfexp(log_fbar));
	REPORT(mfexp(log_fbar+log_ft_dev));
	REPORT(ahat);
	REPORT(ghat);
	REPORT(ct);
	REPORT(ct_hat);
	REPORT(fmsy);
	REPORT(msy);
	REPORT(bmsy);

TOP_OF_MAIN_SECTION
	
	time(&start);
	arrmblsize = 50000000;
	gradient_structure::set_GRADSTACK_BUFFER_SIZE(1.e7);
	gradient_structure::set_CMPDIF_BUFFER_SIZE(1.e7);
	gradient_structure::set_MAX_NVAR_OFFSET(5000);
	gradient_structure::set_NUM_DEPENDENT_VARIABLES(5000);


GLOBALS_SECTION
	/**
	\def REPORT(object)
	Prints name and value of \a object on ADMB report %ofstream file.
	*/
	#undef REPORT
	#define REPORT(object) report << #object "\n" << object << endl;

	#include <admodel.h>
	#include <time.h>
	#include <contrib.h>//IF you have ADMB-11
	//#include<stats.cxx>//If you have ADMB-10 and make sure stats.cxx is in your working directory
	time_t start,finish;
	long hour,minute,second;
	double elapsed_time;

FINAL_SECTION
	time(&finish);
	elapsed_time=difftime(finish,start);
	hour=long(elapsed_time)/3600;
	minute=long(elapsed_time)%3600/60;
	second=(long(elapsed_time)%3600)%60;
	cout<<"*******************************************"<<endl;
	cout<<"--Start time: "<<ctime(&start)<<endl;
	cout<<"--Finish time: "<<ctime(&finish)<<endl;
	cout<<"--Runtime: ";
	cout<<hour<<" hours, "<<minute<<" minutes, "<<second<<" seconds"<<endl;
	cout<<"*******************************************"<<endl;


