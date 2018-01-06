

qd = function(beta0, beta1, p){
  q = beta0 + beta1*p
  return(q)
}


cost = 5 

fnout = list()
nrun=0;for (price in seq(0,10,by=0.25)){
  q = qd(100, -10, price)
  #if(q > 50)  q = qd(100, -15, price)
  nrun=nrun + 1
  fnout[[nrun]] = data.frame(q= q, p = price,  rev = q*price, profit = q*price - q*cost)
}

fnout = do.call("rbind", fnout)


dat = fnout %>% select(-q) %>% gather( key, value, -p, rev)  %>% 
  mutate(key = as.factor(key)) %>% arrange(key)
dat$key2 =  factor(dat$key, levels = rev(levels(dat$key)))

pdf("optprice.pdf")
ggplot(data=dat, aes(x=p, y=value, color=key2)) +
  geom_line()  +
  ylim(0, 300) + 
  xlim(0, 10) + 
  labs(title = "Revenue and Profit", x = "Price", y = "$", subtitle = paste0(
    "Opt rev price of $", fnout[which.max(fnout$rev),]$p, 
    " Opt profit price of $",fnout[which.max(fnout$profit),]$p)
  ) +
  scale_color_manual(labels = c("Revenue ($)", "Profit ($)"), values = c("rev"="darkred","profit"="steelblue")) +
  guides(color=guide_legend("") ) + 
  geom_vline(xintercept=fnout[which.max(fnout$profit),]$p,lty= 3, col = "steelblue") +
  geom_vline(xintercept=fnout[which.max(fnout$rev),]$p,lty= 3, col = "darkred")

dev.off()