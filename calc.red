0.5 + 0.3e2 * 12.5e2 + 0.3e-2 + 12.5 + 0.3 * sqrt(0.05e-2);

log(10,10);
log(9,3);

flatten([1,[4,2],5])
flatten([[[1,2],3],4,[5,6],[8]])

binomial_cml(n,x,p) :=
	if x > 0 then binomial_cml(n,x-1,p) + binomial_pdf(n,x,p)
	else 0;µ¿Ìä
binomial_cml(10,2,0.25)

poisson_pdf(4,0.61)
normal_pdf(1)
exp_pdf(2,1.625)
binomial_pdf(10,2,0.25)
variance([1,2,5,8,9.7])
standard_deviation([1,2,5,8])
CV([1.03,1.02,1.04,1.03,1.05]); -> 1.1026841635396518
CV([0.98,0.95,0.99,1.02,1.00]); -> 2.619874312864373;
standard_deviation([1.03,1.02,1.04,1.03,1.05]);
standard_deviation([0.98,0.95,0.99,1.02,1.00]);

length([1,2,3,4,5])
max([1,2,3,4,5])
min([1,2,3,4,5])
sum([1,2,3,4,5])
product([1,2,3,4,5])


rest(rest([1,2,3]))
is_nil([])
first([1,2])
last([1,2])
cons(nil,[1])

is_list([])
append(nil,[1])
append([1],[2])
append([1,[2,3]],[2,5])
take([1,2,3,4])
skip([1,2,3,4])
reverse([1,2,3,[4,2],5])
map(incr,[1,2,3])
map(sqrt,[1,2,3])

qsort([1,76,32,12,345,3757,8,6,43,56,1232,9,12,3,2,1])
qsort([1,76,32])
qsort(filter(even,[1,76,32,12,345,3757,8,6,43,56,1232,9,12,3,2,1]))
filter(odd,[1232,9,12,3,2,1])

prime(1237)
prime(2047)
prime_each(13,2)
gcd(1223,132)



standardized_death_rate([0.2,0.1,0.1,0.3],[10,20,30,40]) -> 4.75
standardized_death_rate([0.4,0.1,0.1,0.1],[10,20,30,40]) -> 3.25


¡Ö°å³ØÅý·×³Ø¡×p.57
t_value([13,12.9,10.8,14,14.6,13.4,13.9,14.2,15.2,13.9,14.2,11.7,15,14.1],[14,13.7,12.9,15.1,14.5,14.2,13.6,13.4,14.0,13.8,14,14.2]) -> -0.86
t_value([98,88,100,96,107,114],[86,73,95,92,99,116]) -> 2.82



systolic() := 174;
diastolic() := 40;
pulse_pressure()
mean_pressure()

height() := 169;
weight() := 54;
BMI()
Rohrer()
Rohrer(60,170)
BSA()
burn_percentage() := 60;
Baxter()

RBC() := 420;
Hct() := 38;


Na() := 140;
Cl() := 100;
HCO3() := 22;
PaCO2() := 35;
pH()
pH(22,35)

AG()
AG(140,100,22)


comb(8,2) -> 28
comb(52,5) -> 2598960
expt(2,10.9)
expt(2,-2)
log(2,3)
log(2)
exp(1)
expt(sqrt(2),10)
perm(3,2)


¡Ö¿äÂ¬Åý·×¡×p.75
mean([18,19,24,26,28,29,33,35,36,38,38,39,42,44,45,46,46,47,48,48,50,52,53,53,54,54,55,56,56,56,57,59,60,62,63,64,65,66,68,68,69,74,75,75,78,79,83,88,93]);
geometric_mean([18,19,24,26,28,29,33,35,36,38,38,39,42,44,45,46,46,47,48,48,50,52,53,53,54,54,55,56,56,56,57,59,60,62,63,64,65,66,68,68,69,74,75,75,78,79,83,88,93]);
median()([390,395,400,401,406,408,410,415,450]); -> 406
median([390,395,400,401,406,408,410,415,450]); -> 406

¡Ö¿äÂ¬Åý·×¡×p.112
variance([28,45,54,63,78])

real_vals() := [1625,5,1022,1];¼ÂÂ¬ÃÍ
sum(real_vals()) = 2653
rikan() := 5 + 11;
rikanritu() := 0.00603090840557859;
theo_vals() := [1625+5 - 9.830380701093102,9.830380701093102,1023-6.169619298906898,6.169619298906898];ÍýÏÀÃÍ


chi_square(real_vals(),theo_vals())
chi_square([10,5,10,35],[5,10,15,30])

fact(4) == 4 * 3 * 2 * 1;
fact(10) == product([10,9,8,7,6,5,4,3,2,1]);
fact(10); -> 3628800

¡Ö°å³ØÅý·×³Ø¡×p.57
t_value([13,12.9,10.8,14,14.6,13.4,13.9,14.2,15.2,13.9,14.2,11.7,15,14.1],[14,13.7,12.9,15.1,14.5,14.2,13.6,13.4,14.0,13.8,14,14.2]) -> -0.8691
t_value([98,88,100,96,107,114],[86,73,95,92,99,116]) -> 1.0157
mean([13,12.9,10.8,14,14.6,13.4,13.9,14.2,15.2,13.9,14.2,11.7,15,14.1]) -> 13.63571
mean([14,13.7,12.9,15.1,14.5,14.2,13.6,13.4,14.0,13.8,14,14.2]) ->13.95000 
variance([120,130,124,140,130,110,132,120,122,132,128,130,132,126,130]) -> 50.20952
mean([120,130,124,140,130,110,132,120,122,132,128,130,132,126,130]) -> 127.0667


length([13,12.9,10.8,14,14.6,13.4,13.9,14.2,15.2,13.9,14.2,11.7,15,14.1,14,13.7,12.9,15.1,14.5,14.2,13.6,13.4,14.0,13.8,14,14.2]) -> 26
sum([39,38,37,40,38,37]) -> 229
mean([39,38,37,40,38,37]) -> 38.17
variance([39,38,37,40,38,37])
standard_deviation([39,38,37,40,38,37]) -> 1.17
binomial_pdf(4,4,0.6) -> 0.1296
binomial_pdf(4,2,0.6) -> 0.3456
poisson_pdf(0,0.4) -> 0.67032
poisson_pdf(2,0.4) -> 0.05363



MCV()
if ~ MCV() < 0.80 then "microcytic hypochromic anemia"
	else "not microcytic hypochromic anemia";

if ~true then ~false else ~true;
if ((true & false ) | true) then true else false;
~(false | false);

apply(and,[true,true,false,false],[true,false,true,false]) -> [true,false,false,false]
apply(or,[true,true,false,false],[true,false,true,false])  -> [true,true,true,false]
map(not,[true,true,false,false])                           -> [false,false,true,true] 
apply(implies,[true,true,false,false],[true,false,true,false]);
apply(xor,[true,true,false,false],[true,false,true,false])  -> [false,true,true,false]


1 < 0 | 10 == 0;
all(even,[2,4,7])
all(even,[])
exists(even,[2,4,7])
map(lam(i)even(i),[2,4,7])

nth(2,[39,38,37,40,38,37])
nth(10,[39,38,37,40,38,37])
last([39,38,37,40,38,37])
nth(0,[])


Primer of Biostatistics
#2-1
append(append(append(append(append(append(sequence(0,3),sequence(1,11)),sequence(2,4)),sequence(3,4)),sequence(4,2)),sequence(5,4)),[6,7,9,10,11]); -> [0,0,0,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,3,3,3,3,4,4,5,5,5,5,6,7,9,10,11]
mean([0,0,0,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,3,3,3,3,4,4,5,5,5,5,6,7,9,10,11]); -> 3.09
standard_deviation([0,0,0,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,3,3,3,3,4,4,5,5,5,5,6,7,9,10,11]); -> 2.89
#2-2
mean([289,203,359,243,232,210,251,246,224,239,220,211]) -> 244
standard_deviation([289,203,359,243,232,210,251,246,224,239,220,211]) -> 43
#2-6
div(x,y) := x/y;
apply(div,[1.4,1.6,1.2,7.3],map(sqrt,[151,149,157,155])); -> [0.11,0.13,0.10,0.59]
apply(standard_error_mean,[1.4,1.6,1.2,7.3],[151,149,157,155]); -> [0.11,0.13,0.10,0.59]
#3-2
data() := [[200,200,200,200,200],[3.17,2.72,2.63,2.29,2.12],[0.74,0.71,0.73,0.70,0.72]];
nth(0,data());
SEM = [0.05232590180780451,0.05020458146424487,0.05161879502661797,0.049497474683058325,0.050911688245431415];
S2_wit = 1.2965;
mean = 2.586;


DEALE(5,0.50);

tab1() := [[1,2],
	  [3,4]];
tab2() := [[[1,2],[3,4],[6,7]],
            [1,2],[3,4],[6,7]]];
;item(tab1(),[0,0])


and(x,y) := x & y;

cube(x) := x*x*x;
sum_cubes(a,b) := summation(cube,a,incr,b);
sum_cubes(1,10) -> 3025

integral(cube,0,1,0.1);


dx() := 1e-10;
deriv(f) := lam(x)((f(x+dx())-f(x))/dx());
deriv(cube)(5);

average_damp(f) := lam(x) mean([x,f(x)]);
average_damp(square)(10);


complement_seq([a,t,g,c]);
codon2amino([u,u,u]); -> Phe

atomic_number(H);


590{mL} + 495{mL};
590{mL} * 100;
5.88235294118{(km / hr)};

BMI(70{kg},175{cm});
BMI(70000{g},1.75{m});
100.0{mm} / 10.{cm}

coerce(100{mL},1.0{m3})
coerce(100{mL},1100{m3})
standardize(70000{g})
standardize(1.75{m})
standardize(0.1{km})
standardize(60{km} / 1{hr});
standardize(10000{mm})
standardize(1{kPa});
standardize(40000{g * g}); -> 200{g}*200{g} -> 0.2{kg}*0.2{kg} -> 0.04{kg * kg}
standardize((19{drop/mL} * 500{mL}) / 1{hr});
standardize((19{kg/mL} * 500{mL}) / 1{hr});
standardize(12{drop});
standardize(1.0{kg/mL});
standardize(1.0{mg/mL});


conv(1{kg},1.0{g});
conv(1{day},1{hr});
conv(1{day},1{min});
conv(1300{mL/day},1.0{mL/min});

Ccr(65.0{mg/mL},1300{mL/day},1.0{mg/mL});
Ccr(urine_cr,urine_volume,serum_cr) := (urine_cr * urine_volume) / serum_cr;
Ccr(65.0,1300,1.0);

84500.{((mg / mL) * (mL / day))} / 1.0{mg/mL};
84500.{((mg * mL) * mL)};
65.{((mg / mL) * (mg / mL))};
65.0{mg/mL}  / 1.0{mg/mL};
65{((kg / day) / (kg / m))};

combinator
And(odd,even)(2,3);
Curry(and)(true)(false);
Curry(similarity_score)(a)(b);
Id()(1+1);
K()(1+2,1);
;S()(1,2,3);?

matrix(1,2)([[1,2,3],[4,5,6]]);
max_col(make_matrix(2,3,n));

