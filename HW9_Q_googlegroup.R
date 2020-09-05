

### HW9.1 in rethinking 

library(rethinking) 
data(bangladesh)
d <- bangladesh

dat_list <- list(
  C = d$use.contraception,
  did = as.integer( as.factor(d$district) ), 
  urban = d$urban
)

m1.1 <- ulam( 
  alist(
    C ~ bernoulli( p ),
    logit(p) <- a[did] + b[did]*urban,
    c(a,b)[did] ~ multi_normal( c(abar,bbar) , Rho , Sigma ), 
    abar ~ normal(0,1),
    bbar ~ normal(0,0.5),
    Rho ~ lkj_corr(2),
    Sigma ~ exponential(1) 
  ) , data=dat_list , chains=4 , cores=4 )

precis(m1.1)

precis( m1.1 , depth=3 , pars=c("Rho","Sigma") )

## plot individual effects in rethinking

post <- extract.samples(m1.1)

a <- apply( post$a , 2 , mean ) 
b <- apply( post$b , 2 , mean )
plot( a, b , xlab="a (intercept)" , ylab="b (urban slope)" )
abline( h=0 , lty=2 )
abline( v=0 , lty=2 )

## plot in the outcome scale in rethinking

u0 <- inv_logit( a )
u1 <- inv_logit( a + b )
plot( u0 , u1 , xlim=c(0,1) , ylim=c(0,1) , xlab="urban = 0" , ylab="urban = 1" )
abline( h=0.5 , lty=2 )
abline( v=0.5 , lty=2 )


### HW9.1 in INLA 

library(INLA)
library(brinla)

d1.i <- d %>% 
#make a new variable of district that is continuous
  mutate(C = use.contraception,
         did = as.integer( as.factor(d$district)), 
         a.did= did,
         b.did= did + max(did)
  )

n.district= max(d1.i$did) # = 60


m1.1.i <- inla(C ~ 1 + urban + f(a.did, model="iid2d", n= 2*n.district) + f(b.did, urban, copy= "a.did"), data= d1.i, family = "binomial", 
               Ntrials = 1, 
               control.fixed = list(
                 mean= 0,
                 prec= 1/(0.5^2), 
                 mean.intercept= 0, 
                 prec.intercept= 1),
               control.family = list(control.link=list(model="logit")),
               control.predictor=list(link=1, compute=T),
               control.compute=list(config=T, dic=TRUE, waic= TRUE))
summary(m1.1.i)



## plot individual effects in INLA

#m1.1.i$summary.random has 2 elements: a.did and b.did but they are identical.

#extract the means of the random effects with m1.1.i$summary.random[[1]][["mean"]]
m1.1.i.mean <- bind_cols(did= 1:length(m1.1.i$summary.random[[1]][["mean"]]), mean= m1.1.i$summary.random[[1]][["mean"]]) %>% 
  #assign first 60 rows to a.did and 61:120 to b.did in new variable "param" 
  mutate(param= if_else(did <= n.district, "a.did", "b.did"), 
         #new variable "district" assigns the district index 1:60 to both a.did and b.did estimates. 
         district= rep(1:60,2)) %>% 
  #split dataframe by param into a list of 2: a.did and b.did 
  group_split(param) %>% 
  #join the a.did and b.did elements of the list into a dataframe by "district"
  reduce(left_join, by= "district") %>% 
  select(c( "district", "mean.x", "mean.y")) %>% 
  rename("a.did"= "mean.x", "b.did"= "mean.y")



m1.1.i.mean.plot <-  ggplot()+
  geom_point(data= m1.1.i.mean, aes(x=a.did, y= b.did))+
  geom_hline(yintercept=0, linetype='longdash') +
  geom_vline(xintercept = 0)+
  labs(x= "a (intercept)", y = "b (urban slope)")+
  theme_bw()

m1.1.i.mean.plot

## plot in the outcome scale in INLA

inverse_logit <- function (x){
  p <- 1/(1 + exp(-x))
  p <- ifelse(x == Inf, 1, p)
  p }


m1.1.i.outcome <- m1.1.i.mean %>% 
  mutate(u0.i = inverse_logit(a.did), 
         u1.i= inverse_logit(a.did + b.did))


m1.1.i.outcome.plot <-  ggplot()+
  geom_point(data= m1.1.i.outcome, aes(x=u0.i, y= u1.i))+
  geom_hline(yintercept=0.5, linetype='longdash') +
  geom_vline(xintercept = 0.5)+
  labs(x= "urban = 0", y = "urban = 1")+
  xlim(0,1) +
  ylim(0,1)+
  theme_bw()

m1.1.i.outcome.plot


