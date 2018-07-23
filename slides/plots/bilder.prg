/* Programm zum Erzeugen von Bildern fuer das Skript */

graphset;
makewmf;

/*
d=rndmvn(1000,0|-1.2,1~0.8|0.8~1)|rndmvn(1000,1|1,1~-0.7|-0.7~1);
x=grid(-4,4,41);
y=grid(-4,4,39);
f=kernel2(x',y,0.3|0.3,d);
xlabel("X");
ylabel("Y");
zlabel("Dichte f(x,y)");
surface(x',y,f);
waitc;

_pzclr=0|0;
_plotsiz={5.5 5.5};
x=grid(-4,4,101);
y=grid(-4,4,99);
f=kernel2(x',y,0.3|0.3,d);
xtics(-4,4,2,2);
ytics(-4,4,2,2);
contour(x',y,f);

fonts("simplex simgrma");
xlabel("\201Wahrer Parameterwert \202m");
ylabel("\201Powerfunktion G(\202m\201)");
xtics(2,8,1,2);
m=grid(2,8,200);
m0=5;
s=2;
g=1-(cdfn(1.96-sqrt(50)*((m-m0)/s))-cdfn(-1.96-sqrt(50)*((m-m0)/s)));
xy(m,g);

_pltype=5|3|6;
n=2~10~100;
title("Dichte des Stichprobendurchschnitts");
g=grid(-1.5,1.5,500);
legend(0.5,16,"n=2\000n=10\000n=100");
s=1./n;
f=pdfn(g./s)./s;
xy(g,f);

_pltype=5|3|6;
n=2~10~100;
title("Dichte des Schaetzers");
g=grid(-0.5,4,500);
legend(0.5,16,"n=2\000n=10\000n=100");
s=1./n;
f=pdfn((g-2./n)./s)./s;
xy(g,f);

*/

v=zeros(10000,3);
i=0;
do until i==10000;
 i=i+1;
 x=meanc(rndexp(3,1,1))~meanc(rndexp(10,1,1))~meanc(rndexp(100,1,1));
 v[i,.]=x;
endo;
d=grid(0,3.5,250);
k=zeros(250,3);
k[.,1]=kernel(d,&normal,0.1,v[.,1]);
k[.,2]=kernel(d,&normal,0.1,v[.,2]);
k[.,3]=kernel(d,&normal,0.1,v[.,3]);
makewmf;
legend(2.2,1.3,"n=3\000n=10\000n=100");
xlabel("theta");
ylabel("Dichte des Schaetzers");
_pltype=5|3|6;
xy(d,k);
