

F1=input('Input force F1 ? ');
a=input('Input angle between F1 and horizontal line ? ');
F2=input('Input force F2 ? ');
b=input('Input angle between F2 and horizontal line ? ');
M1=F1*cos(a*pi/180);
N1=F1*sin(a*pi/180); 
M2=F2*cos(b*pi/180);
N2=F2*sin(b*pi/180); 
k1=tan(a*pi/180);
k2=tan(b*pi/180); 
x1=0:M1/100:M1;    
y1=k1*x1;
x2=0:M2/100:M2;
y2=k2*x2;
plot(x1, y1, 'k-', x2, y2, 'k-');

% Plot F1 and F2
axis([0 5 -5 5]);
text(M1,N1,'F1');
% Name the force 
text(M2,N2,'F2');
hold on

y1=0:N1/100:N1;
% Projection to x axis
Xf1=M1;
y2=0:N2/100:N2; 
Xf2=M2;
plot(Xf1, y1, 'k:', Xf2, y2, 'k:');
text(Xf1, 0, 'Xf1');
text(Xf2, 0, 'Xf2'); % name the two forces 
hold on
x1=0:M1/100:M1;
% Projection to y axis
Yf1=N1;
x2=0:M2/100:M2;
Yf2=N2; 
plot(x1, Yf1, 'k:', x2, Yf2, 'k:');
text(0, Yf1, 'Yf1');
text(0, Yf2, 'Yf2');
% Superimposed force
hold on;
Xh=M1+M2;
Yh=N1+N2; 
Fh=sqrt(Xh.^2 + Yh.^2);
% 
k=Yh/Xh;
x=0: Xh/100:Xh;
y=k*x;
plot(x,y,'r-'); 
text(Xh,Yh,'F');
hold on
X3=Xh;
% Projection lines of the Superimposed force 
Y3=0:Yh/100:Yh;
Y4=Yh;
X4=0:Xh/100:Xh; 
plot(X3,Y3,'k:',X4,Y4,'k:');
text(Xh,0,'Xh');
text(0,Yh,'Yh');
title('Superimposed force'); 