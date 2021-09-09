#1. the misunderstanding and misuse in the hypothesis testing, see Harlow, Mulaik and Steiger(1997)
#sample size reefers to the number of observations
#the significance level refers to as alpha as a Type I error
#power is defined as one minus probability of making a Type II error. Can be thought as the probability of finding an effect that is there
#effect size is the magnitutde of the effect under the alternate or research hypothesis. The formula for effect size depends on the statistical methodology employed in the hypothesis testing

#our goal is typically to maximize the power of statistical tests while maintaining an aceptable significance level and employning as small as sample size as possible
#the four quantities(sample size, significance level, power and effect size)have an intimate relationship. Given any three, you can determine the fourth

#2. power analysis with the pwr package
pwr.2p.test #two proportions(equal n)
pwr.2p2n.test#rwo proportions(unequal n)
pwr.anova.test#balanced one-way ANOVA
pwr.chisq.test#chi-square test
pwr.f2.test#general linear model
pwr.p.test#proportion(one sample)
pwr.r.test#correlation
pwr.t.test#t tests(one sample, two samples, paired)
pwr.t2n.test#t test(two samples with unequal n)
#of the four quantities, effect size is often the most difficult to specify

#3.t-tests powering
#when the statistical test to be used is a t test.
pwr.t.test(n=, d=, sig.level=, power=, type=, alternative=)
#where n is the sample size
#d is the effect size defined as the standardized mean difference
#sig.level is the significance level(.05 as the default)
#power is the power level
#type is a two-sample t-test("two.sample), a one-sample t-test("one.sample"), or a dependent sample t-test("paired). Two-sample test is the default
#alternative indicates whether the stattistical test is two-sided("two.sided), one sided("less" or "greater"). A two sided test is the default

install.packages("pwr")
library(pwr)
pwr.t.test(d=.8, sig.level=.05, power=.9, type="two.sample",
           alternative="two.sided")
#the results suggest that we need 34 participants in each group(for a total of 68 participants) in order to detect an effect size of .8 with 90% certainty and no more than a 5% chance or erroneously concluding that a difference exists when, in fact, it doesnt

pwr.t.test(n=20, d=.5, sig.level=.01, type="two.sample", alternative="two.sided")
#the power suggested is 0.14


#if the sample sizes for the two groups are unequal, the function
pwr.t2n.test(n1=, n2=, d=, sig.level=, power=, alternative=)#can be used
#here n1 and n2 are the sample size

#4. ANOVA which provides power analysis options for a balanced one-way analysis of variance
#FOR anova we should know f by p,n,N,mean, sd from formala(please search it)
pwr.anova.test(k=, n=, f=, sig.level=, power=)
#where k is the number of groups and n is the common sample size in each group

pwr.anova.test(k=5, f=.25, sig.level=.05, power=.8)
#the total sample size is therefore 5*39 or 195.
#the effect size f requires to estrimate the means of groups as well as the common variance

#5. correlations power
pwr.r.test(n=, r=, sig.level=, power=, alternative=)
#r is the effect size(as measure by a linear correlation coefficient)

pwr.r.test(r=.25, sig.level=.05, power=.90, alternative="greater")

#thus we need to assess in 134 participants in order to be 90% confident

#6. linear model power analysis
pwr.f2.test(u=, v=, f2=, sig.level=, power=)
#where u and v are the numerator and denominator degrees of freedom and f2 is the effect size
#f^2=R^2/(1-R^2) is appropriate when evaluating the impact of a set of predictors on an outcome
#f^2=(Rab^2-Ra^2)/(1-Rab^2), Ra^2 is variance accounted for the population by variable set A, Rab is by variable set A and B together, it is for evaluating the impact of one set of predicts above and beyond a second set of predictos


#if sig level=.05, power-.90, u=3, we can know the effect size using the second formula is f2 =(.35-.30)/(1-.35)=.0769

pwr.f2.test(u=3, f2=.0769, sig.level=.05, power=.90)
#in the multiple regression, the denominator degrees of freedom equals N-k-1, where N is the number of observations and k is the number of predictors
#in this case N-7-1=185 thus required sample size is N=185+7+1

#7. tests of proportions
pwr.2p.test(h=,n=, sig.level=, power=) #where h is the effect size and n is the common sample size in each group
#h=2arcsin(sqrt(p1))-2arcsin(sqrt(p2)) and can be calculated with the function ES.h(p1,p2)

pwr.2p.test(h=ES.h(.65,.6), sig.level=.05, power=.9, alternative="greater")
#we will need a study with 1,605 individuals receiving the new drug and 1,605 receiving the existing drug


#8, chi-square tests

pwr.chisq.test(w=, N=, df=, sig.level=, power=) #where w is the effect size, N is the total sample size and df is the degrees of freedom
#here effect size w is defiend as sqrt(addition((p0i-p1i)^2/p0i)), p0i is cell probability in ith cell under H0, p1i is cell probability in ith cell under H1

#if we are ooking at the relationship between ethnicity and promotion. We anticipate the 70% of the sample will be caucasian and 10% will be african american
#and 20% will be hispanic
#further we believe that 60% of caucasians tend to be promoted and 30% for african americans and 50% for hispanics
#                  promoted         nonpromoted
#caucasian          .42                  .28
#african american   .03                  .07
#hispanic           .10                  .10

#so we expect that 42% of the pop[ulation will be promoted caucasians and 7& of the population will be nonpromoted african americans
#sig.level=.05
#desired power level is .90
#the degrees of freedom in a two-way contingency table are (r-1)*(c-1) where r is the number of rows and c is the number of columns
prob <-matrix(c(.42,.28,.03,.07,.10,.10), byrow=T, nrow=3)
ES.w2(prob)

pwr.chisq.test(w=.1853, df=2, sig.level=.05, power=.9)
#the results suggest that a study with 369 participants will be adequate to detect a relationship between ethnicity and promotion given the effect size, power and alpha

#9. choosing an appropriate effect size in novel situations
#expected effect size is the most difficult parameter to detemine
#it typically requires that we have experience with the subject matter and measures emplyed
#for example, the data from past studies can be used to calculate effect size

#but when research situation is completely novel and we have no past experience to call upon?
#in the area of behavioral science, Cohen attempted to provide benchmarks for "small", "medium" and "large" effect size for various statistical tests

#                        effect size measures       suggested: small   medium   large
#t test                           d                     .20               .50     .80
#ANOVA                            f                     .10               .25     .40
#Linear models                    f2                    .02               .15     .35
#test of proportions              h                     .20               .50     .80
#chi-square                       w                     .10               .30     .50

library(pwr)
es <-seq(.1,.5,.01)
nes <-length(es)

samsize <-NULL
for(i in 1:nes){
  result <- pwr.anova.test(k=5, f=es[i],sig.level=.05, power=.9)
  samsize[i]<-ceiling(result$n)
}
plot(samsize, es, type="l", lwd=2, col="red",
     ylab="Effect Size",
     xlab="Sample Size (per cell)",
main="One Way ANOVA with Power =.90 and Alpha =.05")
#graph such as this can help estimate the impact of various conditions on experimental design
#for example, there appears to be little bang for the buck in increasing the sample size above 200 observations per group

#10. creating power analysis plots
#we would like to see the sample size necessary to declare a correlation coefficient statistically significant for a raneg of effect sizes and power levels

library(pwr)
r <- seq(.1,.5,.01)
nr <-length(r)

p<-seq(.4,.9,.1)
np <-length(p)
samsize <-array(numeric(nr*np), dim=c(nr,np))
for(i in 1:np){
  for(j in 1:nr){
    result <-pwr.r.test(n=NULL, r=r[j],
                        sig.level=.05, power=p[i],
                        alternative = "two.sided")
    samsize[j,i]<-ceiling(result$n)
  }
}
xrange <-range(r)
yrange <-round(range(samsize))
colors <- rainbow(length(p))
plot(xrange, yrange, type="n",
     xlab="Correlation Coefficient (r)",
     ylab="Sample Size (n)")
for(i in 1:np){
  lines(r, samsize[,i], type="l", lwd=2, col=colors[i])
}

abline(v=0, h=seq(0, yrange[2],50), lty=2, col="grey89")
abline(h=0, v=seq(xrange[1], xrange[2],.02), lty=2, col="gray89")

title("Sample Size Estimation for Correlation Studies\nSig=.05 (Two-tailed)")

legend("topright", title="Power", as.character(p), fill=colors)

#use seq function to generate a range of effect sizes r and power levels p
#use loop to calculate the corresponding sample size required and saving in samsize
#the graph is set up with the appropriate horizontal and verticval axes and labels
#power curves are added using lines rather than points


#11. other packgaes for power

asypow#power calculations via asymptotic likelihood ratio methods
longpower#sample size calculations for longitudinal data
PweGSD#popwer analysis for group sequential designs
pamm#power analysis for random effects in mixed models
powerSurvEpi#power and sample size calculations ofr survival analysis in epedemiological studies
powerMediation#power and sample size calculations for mediation effects in linear, logistic, poisson and cox regression
powerpkg#power analyses for the affected sib pair and the TDT design
powerGWASinteraction#power calculations for interactions for GWAS
pedantics#functions to facilitate power analyses for genetic studies of natural populations
gap#functions for power and sample size calculations in case-cohort designs
ssize.fdr#sample size calculations for microarray experiemnts

#MBESS package contains a wide range of functions that can be ussed for various forms of power analysis and sample size determination
#the functions are particularly relevant for researchers in the behavioral, educational and social sciences.
