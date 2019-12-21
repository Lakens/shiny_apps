library(shiny)
# install.packages("pwr") for unequal n
#install.packages("pwr")
library(pwr)  

ui <- fluidPage(
  
 # titlePanel (h2("Misinterpretation of P values [1]",  align = #"right")),
 
# Radiobuttons fixed by Brendan Halpin  "You can't use the same name for multiple inputs. That is, the inputs in the different conditionalPanels need different names. 
  
# version 1.7 -unequal n  
                

#===============================

titlePanel (title= (h3("Calculator for False Positive Risk (FPR)",h4("(version for unequal sample sizes)"),align="center")), 
            
windowTitle=("False Positive Risk calculator (FPR")),

#titlePanel ("Calculator for false positive risk (FPR)"),

sidebarLayout(
  
  sidebarPanel(
    radioButtons("calctype", "Choose what to calculate:",selected = "calcFPR", choices = c(
"1. calculate prior (for given FPR and P value)" = "calcprior"," 2. calculate P value (for given FPR and prior)" = "calcpval",
"3. calculate FPR (for given P value and prior)" = "calcFPR")),
    
    conditionalPanel(
      condition = "input.calctype == 'calcprior'",
      numericInput("pval1", label = h5("Observed P value"),              value = 0.05, min = 0, max = 1, step=0.01),
      numericInput("FPR1", label = h5("False positive risk"), value = 0.05, min = 0, max = 1, step=0.01)
    ),
    
    conditionalPanel(
      condition = "input.calctype == 'calcpval'",
      numericInput("FPR2", label = h5("False positive risk"),             value = 0.05, min = 0, max = 1, step=0.01),
      numericInput("prior2", label = h5("prior probability of          real effect"), value = 0.5, min = 0, max =         1, step=0.01)
    ),
    
  conditionalPanel(
      condition = "input.calctype == 'calcFPR'",
      numericInput("pval3",label = h5("observed P value"),value = 0.05, min = 0, max = 1, step=0.01),
      numericInput("prior3", label = h5("prior probability of real effect"), value = 0.5, min = 0, max = 1, step=0.01)
    ),
    inputPanel(
      numericInput("nsamp1",label = h5("Number in sample 1"), step = 1, value = 16, min = 2)
    ),

inputPanel(
  numericInput("nsamp2",label = h5("Number in sample 2"), step = 1, value = 16, min = 2)
),
inputPanel(
  numericInput("effsize",label = h5("Effect size (as multiple of SD)"),step=0.003,min=0.01, value = 1.0)
),
    
    helpText("Please cite this page if you find it useful: False Positive Risk Web Calculator, version 1.7 Longstaff,C. and Colquhoun D, http://fpr-calc.ucl.ac.uk/ Last accessed", Sys.Date())
  ),

#==========================
  mainPanel( 
    tabsetPanel(type="tab",
                tabPanel("Calculations", 
                         
                         helpText(h4("Results")),
                         
                         h5(textOutput("text3")), tableOutput("resultsTable")),
                
                
                
                tabPanel("Notes",
                         
helpText(h3("False positive risk calculations")),                
helpText("This web app was written by Colin Longstaff and David Colquhoun with help from Brendan Halpin. And in ver 1.7, help from Dr Will Parry to fix default for nsamp2."), 

helpText(h4("Statistical considerations")),
helpText("The question which we ask in refs[1 - 4, 6, 7] is as follows. If you observe a \"significant\" P value after doing a single unbiased experiment, what is the probability that your result is a false positive? \n
An account of the precise assumptions that underlie the calculations is given in ref [7]."),

helpText("\"False positive risk\" (FPR) is defined here as the probability that a result which is \"significant\" at a specified P value, is a false positive result. It is defined and explained in refs [3, 7 and at 26' in ref 6]. The same thing was called \"false discovery rate\"in refs [1] and [2], and it was called \"false positive rate\" in earlier drafts of refs [3, 7]   The notation in this field is a mess and it's important to check the definitions in each paper"),

helpText("There are two different ways to calculate FPR. These are explained in detail in section 10 of ref [1], and, more carefully, in section 3 of ref [3].  They  can be called the ", tags$em("p-equals"),"method, and the ", tags$em("p-less-than"), "method. The latter definition is used most frequently (eg by Ioannidis and by Wacholder), but the former method is more appropriate for answering our question. All three options give results that are calculated with both methods. The results with the ",tags$em("p-equals"),"method, give a higher false positive risk, for any given P value, than the other method (see Fig 2 in ref [3]), but they are the appropriate way to answer the question."),

helpText(h4("How to run calculations")),

helpText("Click on the calculations tab, and choose which calculation to do by selecting one of the three radio buttons (top left)  The input boxes that are appropriate for the calculation will appear. There are three variables, observed P value, FPR and the prior probability that the null hypothesis is false. The calculator will work out any one of these, given numbers for the other two.  All three calculations require also the number of observations in each sample,  and the effect size, expressed as a multiple of the standard deviation of the observations (default value 1.0)  The default number per sample is 16 which gives a power of 0.78 for P = 0.05 and effect size = 1 -see refs [1] and [3] for more details."  
),
helpText(" Note that all that matters is the effect  size expressed as a multiple of the standard deviation of the original observations (sometimes known as",tags$em("Cohen's d"),").  The true mean of sample 1 is always 0 (null hypothesis), The true mean of sample 2 is set to the normalised effect size so the true standard deviation can always be set to 1, with no loss of generality."   
),

helpText(h4("A real life example")),

helpText("Fully worked examples are given in section 8 of ref [7].",
tags$br(),
" A study of transcranial electromagnetic stimulation, published In ",tags$em("Science")," concluded that it \"improved associative memory performance\", P = 0.043.  If we assume that the experiment had adequate power (the sample size of 8 suggests that might be optimistic)  then, in order to achieve a false positive risk of 5% when we observe P = 0.043, we would have to assume a prior probability of 0.85 that the effect on memory was genuine (found from radio button 1). Most people would think it was less than convincing to present an analysis based on the assumption that you were almost certain (probability 0.85) to be right before you did the experiment.",
tags$br(),
"Another way to express the strength of the evidence provided by P = 0.043 is to note that it makes the existence of a real effect only 3.3 times as likely as the existence of no effect (likelihood ratio).  This would correspond to a minimum false positive risk of 23% if we were willing to assume that non-specific electrical zapping of the brain was as likely as not to improve memory (prior probability of a real effect was 0.5) (found via radio button 3).",
tags$br(),
"The radio button 2 option shows that in the most optimistic case (prior = 0.5), you need to have P = 0.008 to achieve an FPR of 5 percent. (Example from refs [3] and [7].) "  
         
),

helpText(h4("Matching power")),

helpText(" Much the same results are found for FPR if the power is kept constant. This is explained and exemplified in section 5 and Figure 1 of ref [7].",
tags$br(),
"For example effect size = 1 and n = 16 gives power = 0.78. For an effect size of 0.5 SD, n = 61 gives similar power and also a similar FPR etc.  And for an effect size of 0.2 SD, a power of 0.78 requires n = 375, and again this gives similar FPR etc. See ref [7] for more details. \n 
So choose n so that the calculated power matches that of your experiment."
),
helpText("There is a popular account of the logic involved in ref [4]. And ref [3] has, in section 9, a response to the recent 72 author paper, Benjamin et al  [5], on related topics.  There is a more technical account of the assumptions in ref [7]. And a defence of those assumptions in ref [8]."),
helpText(h4("Versions")),
helpText("From ver 1.1 onwards, the effect size (expressed as a multiple of the standard deviation of the observations) can be entered. From ver 1.3 onwards, the values of power that are printed out are calculated for P = 0.05 and the specified effect size (expressed as a multiple of the standard deviation of the observations).  In earlier versions they were calculated using the  observed P value).\n Ver 1.4 has updated help notes. \n Ver 1.5 has updated help notes and references. \n Ver 1.6 is unchanged apart from the default radio button selected at start-up is now button 3, rather than button 1.\n Ver 1.7 allows unequal sample sizes (but still assumes same  variance for both samples)."
),

helpText(h4("References")),

helpText(
  "1.	Colquhoun D.(2014) An investigation of the false discovery rate and the misinterpretation of p-values.",tags$em(" Royal Society Open Science"), "1(3):140216. doi: 10.1098/rsos.140216.",
  tags$a(href="http://rsos.royalsocietypublishing.org/content/1/3/140216", "Click for full text"),
 tags$br(),
"2. 	Colquhoun D. False discovery rates: the movie (now superseded by ref [6])",
tags$a(href="https://www.youtube.com/watch?v=tRZMD1cYX_c", "Click for YouTube"),
tags$br(),
"3. 	Colquhoun D. (2017). The reproducibility of research and misinterpretation of P values.",
tags$em("Royal Society Open Science"),
" 4 (12), doi: ttp://dx.doi.org/10.1098/rsos.171085",
tags$a(href="https://royalsocietypublishing.org/doi/full/10.1098/rsos.171085", "Click for full text"),
tags$br(),
"4. 	Colquhoun D. (2016). The problem with p-values. Aeon Magazine",
tags$a(href="https://aeon.co/essays/it-s-time-for-science-to-abandon-the-term-statistically-significant", "Click for full text"),
tags$br(),
"5. 	Benjamin, D. et al. (2017)  Redefine Statistical Significance. PsyArXiv Preprints, July 22, 2017.", tags$a(href="https://dx.doi.org/10.17605/OSF.IO/MKY9J", "Click for full text"), 
tags$br(),

"6. Colquhoun, D. (2018) Colquhoun D. The false positive risk: a proposal concerning what to do about p-values (version 2) [talk based on that given at EvidenceLive, 2018]",
tags$a(href="https://www.youtube.com/watch?v=jZWgijUnIxI", "Click for YouTube"),
tags$br(),
"7.  Colquhoun, D. (2019)  The false positive risk: a proposal concerning what to do about p values.", tags$em("American Statistician"),".", tags$a(href="https://www.tandfonline.com/doi/full/10.1080/00031305.2018.1529622", "Click for full text"), 
tags$br(),
"8.  Colquhoun, D. (2019b)  A response to critiques of \"The reproducibility of research and the misinterpretation of p-values\".", tags$em("Royal Society Open Science"),".", tags$a(href="https://arxiv.org/ftp/arxiv/papers/1905/1905.08338.pdf", "Click for full text"),  
tags$br(),
 tags$br(),
 "A list of all of DC's publications on p values can be found at ", tags$a(href="http://www.onemol.org.uk/?page_id=456", "Some papers about p values.")



)    # end of helpText references
)    #end of tabpanel
)
  
)   
)
)

#=========================================
server <-  function(input, output,session){
 # *** observeEvent statement inserted on advice of Dr Will Parry (and 'session' added to line above)
   observeEvent(input$nsamp1, { #watch for change in first input...
    #if it changes, update second input to same value...
    updateNumericInput(session, "nsamp2", value = input$nsamp1) 
  }) 
  # *** end of insertion
  
  output$resultsTable<-renderTable({
    
    if (input$calctype == "calcprior") {
      pval = input$pval1
      FPR = input$FPR1
      prior = NaN
    }
    if (input$calctype == "calcpval") {
      pval = NaN
      FPR = input$FPR2
      prior = input$prior2
    }
    if (input$calctype == "calcFPR") {
      pval = input$pval3
      FPR = NaN
      prior = input$prior3
    }
    #
    nsamp1=input$nsamp1   
    nsamp2=input$nsamp2   
    #    
    mymu1=0
    mymu2=input$effsize
    mysd1=1
    mysd2=1
    sigma=1
    delta1=mymu2-mymu1
    sdiff=sqrt(sigma^2/nsamp1 + sigma^2/nsamp2)
    #    sdiff=sqrt(input$sigma^2/input$nsamp + sigma^2/input$nsamp)
    df=(nsamp1-1)+(nsamp2-1)
    # Note FPR doesn't need calculation of power for p-equals case  
    #
      if (input$calctype == "calcprior") {
      #under H0, use central t distribution
      tcrit=qt((1-pval/2),df,ncp=0)
      x0=tcrit
      y0=dt(x0,df,0)
      #
      # under H1 use non-central t distribution
      ncp1=delta1/sdiff     #non-centrality paramater
      x1=x0  #tcrit
      y1=dt(x1,df,ncp=ncp1)
      # check solution
      #  pcheck=pt(y1,df,ncp=ncp1)
      #  pcheck
      
      p0=2*y0
      p1=y1
      #Rearrange result for FPR to calculate prior for given pval
      prior=(p0*(1-FPR))/(p1*FPR + (p0*(1-FPR)))
      myp=pwr.t2n.test(n1= nsamp1, n2= nsamp2, d=delta1, sig.level= pval, alternative = "two.sided")
#      myp1=power.t.test(n=nsamp,sd=sigma,delta=delta1,sig.level=pval,type="two.sample",ratio=1,alternative="two.sided",power=NULL)
      power = myp$power
      prior0=pval*(1-FPR)/(pval*(1-FPR) + power*FPR)
      prior1=round(prior,4)    #rounded to 4 sig figs
      prior10=round(prior0,4)
      power1=round(power,4)
# For print, calculate power for p=0.05 and specified eff size 
      myp2=pwr.t2n.test(n1= nsamp1, n2= nsamp2, d=delta1, sig.level= 0.05, alternative = "two.sided")
#      myp2=power.t.test(n=nsamp,sd=sigma,delta=delta1,sig.level=0.05,type="two.sample",alternative="two.sided",power=NULL)
      power2 = myp2$power
      power21=round(power2,4)
#      
      ResMat<-matrix(c("INPUT", "", "", "","",
      "FPR", FPR, "","",
      "Observed P value", pval, "",
        
"Sample 1: mean, sd, n1", mymu1, mysd1, nsamp1,
"Sample 2: mean, sd, n2", mymu2, mysd2,nsamp2,
" Effect size (mult of SD)",input$effsize,"",
"",
"","","","",
"OUTPUT", "p-equals case",  "P-less-than case","",
"prior prob of H1", prior1,prior10,"",
"power (for p = 0.05 and effect size)", power21, power21,""),
ncol = 4, nrow = 10,byrow=TRUE)
      
      
    }    #end of if (input$calctype == "calcprior")
    #
    #===================================
    if (input$calctype == "calcpval") {
      pguess=c(10^-8, 0.9999999999)   # pval must be between 0 and 1.
      #Define function to calculate FPR 
      calc.FPR =  function(pval,nsamp1,nsamp2,prior,sigma,delta1)
      {
        sdiff=sqrt(sigma^2/nsamp1 + sigma^2/nsamp2)
        df=(nsamp1-1)+(nsamp2-1)
        # Note FPR doesn't need calculation of power for p-equals case  #  
        #under H0, use central t distribution
        tcrit=qt((1-pval/2),df,ncp=0)
        x0=tcrit
        y0=dt(x0,df,0)
        #
# under H1 use non-central t distribution
        ncp1=delta1/sdiff     #non-centrality paramater
        x1=x0  #tcrit
        y1=dt(x1,df,ncp=ncp1)
        #
        # Calc false positive rate
        p0=2*y0
        p1=y1
        LRH1=p1/p0
        FPR=((1-prior)*p0)/(((1-prior)*p0) + prior*p1)
        FPR
        output=c(FPR,LRH1)
        return(output)
      }
      # end of function calc.FPR
      #===============================
      # Now calc.FPRO, for the p-less-than case
     
      calc.FPR0 =  function(pval,nsamp1,nsamp2,prior,sigma,delta1){
        myp=pwr.t2n.test(n1= nsamp1, n2= nsamp2, d=delta1, sig.level= pval, alternative = "two.sided")
#        myp=power.t.test(n=nsamp,sd=sigma,delta=delta1,sig.level=pval,type="two.sample",alternative="two.sided",power=NULL)
        power = myp$power
        PH1=prior
        PH0=1-PH1
        FPR0=(pval*PH0/(pval*PH0 + PH1*power))
        LR0=power/pval
        output=c(FPR0,LR0,power)
        return(output)
      }
      # end of function calc.FPR0  
      #========================================
      
      
      #
      # Define function, f.root, of pval for given FPR, prior etc. Solve for FPR = FPR1 using uniroot()
      #   calc.FPR =  function(pval,nsamp1,nsamp2,prior,sigma,delta1)
      f.root = function(pval,nsamp1,nsamp2,prior,sigma,delta1)
      {
        FPRcalc=calc.FPR(pval,nsamp1,nsamp2,prior,sigma,delta1)
        x= FPRcalc[1]-FPR   #=0 when FPRcalc = FPR!
        return (x)
      }
      
      plow=f.root(pguess[1],nsamp1,nsamp2,prior,sigma,delta1)
      phi=f.root(pguess[2],nsamp1,nsamp2,prior,sigma,delta1)
      
      if(plow*phi>0) {
        pval1=NaN
        LR1=NaN
      } else { 
        out1=uniroot(f = f.root,nsamp1=nsamp1,nsamp2=nsamp2,prior=prior,sigma=sigma,delta1=delta1,interval=pguess, tol=10^-10, maxiter=10000, check.conv=TRUE)
        pval1=out1$root   #pval that gives FPR= FPR
        # calc.FPR =  function(pval,nsamp1,nsamp2,prior,sigma,delta1)
        out11=calc.FPR(pval1,nsamp1,nsamp2,prior,sigma,delta1)
        LR1=out11[2]
      }
#====================
# repeat above for p-less-than case     
      
      f.root0 = function(pval,nsamp1,nsamp2,prior,sigma,delta1)
      {
      FPRcalc0=calc.FPR0(pval,nsamp1,nsamp2,prior,sigma,delta1)
      x= FPRcalc0[1]-FPR   #=0 when FPRcalc0 = FPR!
        return (x)
      }
      
      plow=f.root0(pguess[1],nsamp1,nsamp2,prior,sigma,delta1)
      phi=f.root0(pguess[2],nsamp1,nsamp2,prior,sigma,delta1)
      
      if(plow*phi>0) {
        pval10=NaN
        LR10=NaN
      } else { 
        out10=uniroot(f = f.root0,nsamp1=nsamp1,nsamp2=nsamp2,prior=prior,sigma=sigma,delta1=delta1,interval=pguess, tol=10^-10, maxiter=10000, check.conv=TRUE)
        pval10=out10$root   #pval that gives FPR= FPR
        # calc.FPR =  function(pval,nsamp1,nsamp2,prior,sigma,delta1)
        out110=calc.FPR0(pval10,nsamp1,nsamp2,prior,sigma,delta1)
        LR10=out110[2]
      }
 #
#      
#==========================      
      pval2=round(pval1,6)    # 6 sig figs
      LR2=round(LR1,4)        # 4 sig figs
      pval20=round(pval10,6)    # 6 sig figs
      LR20=round(LR10,4)  
      
      ResMat<-matrix(c("INPUT", "", "","",
      "FPR", FPR, "","",
      "Prior prob of H1=", prior, "","",
      "Sample 1: mean, sd, n1", mymu1, mysd1, nsamp1,
     "Sample 2: mean, sd, n2", mymu2, mysd2,nsamp2,
   " Effect size (mult of SD)",input$effsize,"","",
"","","","",            
 "OUTPUT", "p-equals case", "p-less-than case","",
"p value", pval2, pval20,"",
"Lik, ratio, L(H1)/L(H0)", LR2,LR20,""),
nrow = 10,ncol=4,byrow=TRUE)
      
    }    #end of if (input$calctype == "calcpval")
    
    #=================================
    if (input$calctype == "calcFPR") {
      #under H0, use central t distribution
      tcrit=qt((1-pval/2),df,ncp=0)
      x0=tcrit
      y0=dt(x0,df,0)
      #
      # under H1 use non-central t distribution
      ncp1=delta1/sdiff     #non-centrality paramater
      x1=x0  #tcrit
      y1=dt(x1,df,ncp=ncp1)
      
      #   Calc false positive risk
      p0=2*y0
      p1=y1
      FPR=((1-prior)*p0)/(((1-prior)*p0) + prior*p1)
      #FPR
      LR1=p1/p0
      
      myp4=pwr.t2n.test(n1= nsamp1, n2= nsamp2, d=delta1, sig.level= pval, alternative = "two.sided")      
#      myp4=power.t.test(n=nsamp,sd=sigma,delta=delta1,sig.level=pval,type="two.sample",alternative="two.sided",power=NULL)
      power = myp4$power
      PH1=prior
      PH0=1-PH1
      FPR0=(pval*PH0/(pval*PH0 + PH1*power))
      LR10=power/pval
      #
      power1 = round(power,4)
      LR2=round(LR1,4)
      LR20=round(LR10,4)
      FPR1=round(FPR,4)
      FPR10 = round(FPR0,4)
      # Print power for p = 0.05 and specified effect size
      mypp=pwr.t2n.test(n1= nsamp1, n2= nsamp2, d=delta1, sig.level= 0.05, alternative = "two.sided")
#      mypp=power.t.test(n=nsamp,sd=sigma,delta=delta1,sig.level=0.05,type="two.sample",alternative="two.sided",power=NULL)
      powerp = mypp$power
      powerp1 = round(powerp,4)
      #
  # 4 columns and 10 rows    
      ResMat<-matrix(c("INPUT", "", "","",      "Observed p value", pval, "","",               "prior prob of H1", prior, "","",               
"Sample 1: mean, sd, n1", mymu1, mysd1,nsamp1,
"Sample 2: mean, sd, n2", mymu2, mysd2,nsamp2,
" Effect size (mult of SD)",input$effsize,"", "",
"","","","",
"OUTPUT","p-equals case","p-less-than case","",
"FPR", FPR1, FPR10,"",
"Likelihood ratio", LR2, LR20,"",
"power (for p = 0.05 and effect size)", powerp1, powerp1,""),
 nrow = 11,ncol=4,byrow=TRUE)
      
    }    # end of  if (input$calctype == "calcFPR")
    #
    
#   write.table(ResMat, "clipboard", sep="\t", col.names=T,row.names=F)  #"clipboard works locally only
    
    colnames(ResMat) = c(" ", " "," "," ")
    write.table(ResMat, sep="\t", col.names=T,row.names=F)
#   as.matrix(ResMat)    
    ResMat

  }
  )
}

#======================================================

shinyApp(ui = ui, server = server)
