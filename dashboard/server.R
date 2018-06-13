qcdata <- qcdata_ssheet[["features"]]
ssheet <- qcdata_ssheet[["ssheet"]]
pc <- as.data.frame(qcdata_ssheet[["pc"]])
pc_sum <- qcdata_ssheet[["pcsum"]]
facs <- qcdata_ssheet[["norm_facs"]]
control <- qcdata_ssheet[["control"]]
housekeep <- qcdata_ssheet[["housekeep"]]

server <- function(input, output){
  observeEvent(input$do,{
    ggsave(paste0(getwd(),"/",input$name,".pdf"), width = input$w, height = input$h)
  })
  
  #Render subtabs based on maintab
  output$subtab <- renderUI({
    req(input$maintabs)
    if(input$maintabs == "met"){
      tabsetPanel(id = "tabs",
                  tabPanel("Binding Density", value = "BD"),
                  tabPanel("Imaging",value="FoV"),
                  tabPanel("Positive Control Linearity",value="PC"),
                  tabPanel("Limit of Detection", value="LoD"))
    }else if (input$maintabs == "cg"){
      tabslist <- unique(control$CodeClass)
      tabslist <-  c(tabslist,"Control probe expression")
      do.call(tabsetPanel,c(id="tabs", lapply(tabslist, function(i){
        tabPanel(title=i)
      })))
    }else if (input$maintabs == "norm"){
      tabsetPanel(id = "tabs",
                  tabPanel("Positive factor vs Background threshold",value="pfbt"),
                  tabPanel("Housekeeping factor", value="hf"),
                  tabPanel("Normalization result", value="norm_res"))
    }else if (input$maintabs=="vis"){
      tabsetPanel(id = "tabs",
                  tabPanel("Average Count vs Binding Density", value="MC-BD"),
                  tabPanel("Average Count vs Median Count", value="MC-MedC"),
                  tabPanel("Principal Component", value="prin"))
    }else{
      
    }
  })
  #Render interface options based on maintab
  output$interfaceA <- renderUI({
    req(input$maintabs)
    if(input$maintabs == "met"){
      selectInput("Attribute",label="Select x-axis",
                  choices = c("Date",
                              "ID",
                              "CartridgeID",
                              "ScannerID",
                              "StagePosition"))
    }else if(input$maintabs == "vis" | input$maintabs == "norm" | input$maintabs=="cg"){
      radioButtons("color_choice", label="Select colorization based on:",
                   choiceNames= c("RCC attributes","Samplesheet attributes"), choiceValues = c(F,T))
    }
  })
  output$interfaceB <- renderUI({
    req(input$maintabs)
    if(input$maintabs == "met"){
      selectInput("meta",label="Color by:",
                  choices = colnames(ssheet[sapply(ssheet,function(y) nlevels(y)<= 20)]))

    }else if(input$maintabs == "vis" | input$maintabs == "norm" |input$maintabs =="cg"){
      color <- ifelse(is.null(input$color_choice),T,input$color_choice)
      if(color){
        selectInput("meta",label="Color by:",
                    choices = colnames(ssheet[sapply(ssheet,function(y) nlevels(y)<= 20)]))
      }else{
        selectInput("Attribute",label="Color by:",
                    choices = c("Date",
                                "ID",
                                "CartridgeID",
                                "ScannerID",
                                "StagePosition"))
      }
    }
  })
  
  output$interfaceC <- renderUI({
    req(input$maintabs)
    if(req(input$maintabs) == "met"){
      checkboxInput("outlier","View outliers", value = T)
    }else if(req(input$maintabs) == "vis"){
      req(input$tabs)
      if (input$tabs == "prin"){
        selectInput("pcA_sel",label="PC on x-axis:",
                    choices = colnames(pc))
        }
    }
  })
  output$interfaceD <- renderUI({
    req(input$maintabs)
    if(input$maintabs == "met"){
      checkboxInput("outlab","View outlier labels")
    }else if(input$maintabs == "vis"){
      req(input$tabs)
      if (input$tabs == "prin"){
        selectInput("pcB_sel",label="PC on y-axis:",
                    choices = colnames(pc), selected = colnames(pc)[2])
        }
      }
  })
  output$interfaceE <- renderUI({
    req(input$maintabs)
    if(input$maintabs == "met"){
      sliderInput("outsize","Outlier size", min= 1, max=10, value = 2)
    }
  })
  output$interfaceF <- renderUI({
    req(input$maintabs)
    req(input$tabs)
    if(input$maintabs == "met"){
      if(input$tabs == "BD" ){
        radioButtons("BD_choice", label="Select instrument",
                     choiceNames= c("MAX/FLEX","SPRINT"), choiceValues = c(2.25,1.8))
      }
    }
    
  })
  output$interfaceG <- renderUI({
    req(input$maintabs)
    req(input$tabs)
    req(input$BD_choice)
    if(input$maintabs == "met"){
      ranges <- c("BD" = c(0.1,4,0.1,input$BD_choice),
                  "FoV" = c(50,100,75),
                  "LoD" = c(0,30,2),
                  "PC" = c(0.5,1,0.95))
      if(input$tabs == "BD"){
        sliderInput("threshold","Custom QC threshold",
                    min= as.numeric(ranges["BD1"]),max = as.numeric(ranges["BD2"]),
                    value = c(as.numeric(ranges["BD3"]),as.numeric(ranges["BD4"])))
      }else if(input$tabs == "FoV" | input$tabs == "LoD" | input$tabs == "PC"){
        min <- as.numeric(ranges[paste0(input$tabs,"1")])
        max <- as.numeric(ranges[paste0(input$tabs,"2")])
        default <- as.numeric(ranges[paste0(input$tabs,"3")])
        sliderInput("threshold","Custom QC threshold",
                    min= min,max = max, value = default)
        }
    }
  })
  
  output$outlier_table <- renderDataTable({
    qc <- qcdata_ssheet[[1]]
    ssheet <- qcdata_ssheet[[2]]
    factors <- qcdata_ssheet[[5]]
    binding_out <- rownames(qc[as.numeric(qc$BD) > 2.25 |
                                 as.numeric(qc$BD) < 0.1,])
    fov_out <- rownames(qc[as.numeric(qc$FoV) < 75,])
    pc_out <- rownames(qc[as.numeric(qc$PC) < 0.95,])
    lod_out <- rownames(qc[as.numeric(qc$LoD) < 2,])
    fac_out <- rownames(factors[factors$Positive_factor < (1/4) |
                                  factors$Positive_factor > 4,])
    house_out <- rownames(factors[factors$House_factor < (1/11)|
                                    factors$House_factor >11,])
    all_out <- unique(c(binding_out,fov_out,pc_out,lod_out,fac_out,house_out))
    
    outlier_table <- data.frame("Accession" = all_out,
                                "BD" = ifelse(all_out %in% binding_out,"FAIL","PASS"),
                                "FOV" = ifelse(all_out %in% fov_out,"FAIL","PASS"),
                                "PL" = ifelse(all_out %in% pc_out,"FAIL","PASS"),
                                "LOD" = ifelse(all_out %in% lod_out,"FAIL","PASS"),
                                "PSF" = ifelse(all_out %in% fac_out,"FAIL","PASS"),
                                "HSF" = ifelse(all_out %in% house_out,"FAIL","PASS"))
    outlier_table
  })
  
  
  output$pcimp <- renderPlot({
    sub <- ifelse(is.null(input$tabs),"BD",input$tabs)
    if(sub == "prin"){
      local_sum <- as.data.frame(t(pc_sum))
      ggplot(local_sum, aes(x=as.factor(1:10), y=local_sum[,2],group=1)) +
        geom_point(color="#ff6961", na.rm=TRUE) +
        geom_line(size=1, color="#ff6961") +
        ylab("Proportion of variance")+
        xlab("Number of principal component") + 
        theme(axis.text=element_text(size=10),axis.title=element_text(size=14,face="bold"))
    }
  })
  
  output$all <- renderPlot({
    #Prepare axis text
    labels <- c("MC" = "Average Counts",
                "MedC" = "Median Counts",
                "BD" = "Binding Density",
                "FoV" = "Field of View",
                "PC" = "Positive Control linearity",
                "LoD"= "Limit of Detection")
    units <- c("BD" = "(Optical features / μm²)",
               "FoV" = "(%Counted)",
               "PC" = "(R²)",
               "LoD" = "(Z)")
    #Set defaults
    req(input$maintabs)
    main <- ifelse(is.null(input$maintabs),"met",input$maintabs)
    if(main == "met"){
      req(input$tabs)
      req(input$threshold)
      #Defaults for every main tab
      if(input$tabs == "BD"){
        local <- qcdata[as.numeric(qcdata[,input$tabs]) >= input$threshold[1] &
                          as.numeric(qcdata[,input$tabs]) <= input$threshold[2],]
        localsheet <- ssheet[as.numeric(qcdata[,input$tabs]) >= input$threshold[1] &
                               as.numeric(qcdata[,input$tabs]) <= input$threshold[2],]
        outliers <- qcdata[as.numeric(qcdata[,input$tabs]) <= input$threshold[1] |
                             as.numeric(qcdata[,input$tabs]) >= input$threshold[2],]
        
      }else if(input$tabs == "FoV" | input$tabs == "LoD" | input$tabs == "PC"){
        local <- qcdata[as.numeric(qcdata[,input$tabs]) >= input$threshold[1],]
        localsheet <- ssheet[as.numeric(qcdata[,input$tabs]) >= input$threshold[1],]
        outliers <- qcdata[as.numeric(qcdata[,input$tabs]) <= input$threshold[1],]
        
      }
      req(input$tabs == "FoV" | input$tabs == "LoD" | input$tabs == "PC" | input$tabs == "BD")
      local[,input$Attribute] <- as.factor(local[,input$Attribute])
      local[,input$Attribute] <- factor(local[,input$Attribute],
                                        mixedsort(levels(local[,input$Attribute])))
      x <- local[,input$Attribute]
      x_out <- outliers[,input$Attribute]
      
      if(length(unique(localsheet[,input$meta])) > 40){
        if(all(!is.na(as.numeric(na.omit(localsheet[,input$meta]))))){
          color <- as.numeric(localsheet[,input$meta])
        }
      }else{
        color <- as.factor(localsheet[,input$meta])
      }
      
      p <- ggplot(local, aes(x=x,y=as.numeric(local[,input$tabs]), color=color)) +
        geom_quasirandom(width=0.2, size=1.5, na.rm=TRUE) +
        theme(legend.position="bottom", axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) + 
        labs(x=input$Attribute,y= paste(labels[input$tabs],units[input$tabs]), color=input$meta)
      
      if(input$tabs == "BD"){
        p <- p + geom_hline(aes(yintercept = input$threshold[1]),colour="red",linetype="longdash", na.rm=TRUE) +
          geom_hline(aes(yintercept = input$threshold[2]),colour="red",linetype="longdash", na.rm=TRUE)
      }else{
        p <- p + geom_hline(aes(yintercept = input$threshold[1]),colour="red",linetype="longdash", na.rm=TRUE)
      }
      
      #Show outlier
      if(input$outlier){
        p <- p + geom_beeswarm(data = outliers,
                               aes(x =x_out,y=as.numeric(outliers[,input$tabs])),
                               colour="red", size=input$outsize, na.rm=TRUE)
      }
      #Show outlier label
      if(input$outlab){
        for (i in 1:nrow(outliers)){
          p <- p + annotate("text",
                            x=outliers[i,input$Attribute],
                            y=(as.numeric(outliers[i,input$tabs])),
                            label=rownames(outliers)[i],
                            hjust=-(sqrt(input$outsize)/10))
        }
      }
      
    }else if(main == "cg"){
      req(input$color_choice)
      req(input$meta)
      req(input$Attribute)
      req(input$tabs)
      if(input$tabs == "Control probe expression"){
        local_data <- control[control$CodeClass %in% c("Positive","Negative"),]
        p <- ggplot(local_data, aes(x=as.numeric(as.factor(L1)), y=log10(as.numeric(Count)), colour=Name)) +
             geom_line() + facet_wrap(~CodeClass,scales = "free_y") + labs(x="Sample index",y="Log10(counts)")
        
      }else{
        if(input$color_choice){
          color_name <- input$meta
          tmp <- data.frame("Accession" = rownames(ssheet),
                            color_name = ssheet[,color_name])
        }else{
          color_name <- input$Attribute
          tmp <- data.frame("Accession" = rownames(qcdata),
                            color_name = qcdata[,color_name])
        }
        levels(tmp[,2]) <- mixedsort(levels(tmp[,2]))
        local <- control[control$CodeClass %in% input$tabs,]
        tmp2 <- sapply(local$L1, function(x) tmp[tmp[,"Accession"] == x,2])
        local[,"color"] <- tmp2
        req(local$color)
        if(length(unique(local$color)) > 40){
          if(all(!is.na(as.numeric(na.omit(local$color))))){
            fill <- as.numeric(local$color)
          }
        }else{
          fill <- as.factor(local$color)
        }
        
        local$Name <- as.factor(local$Name)
        local$Name <- factor(local$Name,levels(local$Name)[c(1:8,14,13,12,11,10,9)])
        p <- ggplot(local, aes(x=Name, y = log10(as.numeric(Count)+1), color=fill)) +
          geom_quasirandom(width = 0.5, na.rm=TRUE) + labs(color=color_name,x="Gene Name", y="log10(Counts)") +
          theme(axis.text.x=element_text(face="italic"))
      }
      
      
      
    }else if(main == "vis"){
      req(input$tabs)
      req(input$color_choice)
      req(input$Attribute)
      req(input$meta)
      if(input$color_choice){
        if(length(unique(ssheet[,input$meta])) > 40){
          if(all(!is.na(as.numeric(na.omit(ssheet[,input$meta]))))){
            fill <- as.numeric(ssheet[,input$meta])
          }
        }else{
          fill <- as.factor(ssheet[,input$meta])
        }
        legendname <- input$meta
      }else{
        fill <- as.factor(qcdata[,input$Attribute])
        levels(fill) <- mixedsort(levels(fill))
        legendname <- input$Attribute
      }
      
      if(input$tabs == "prin"){
        req(input$pcA_sel)
        req(input$pcB_sel)
        #PRINICIPAL COMPONENT PLOT
        p <- ggplot(pc, aes(x=pc[,input$pcA_sel],y=pc[,input$pcB_sel],color=fill)) +
          geom_point(na.rm=TRUE) +
          xlab(as.character(input$pcA_sel))+ ylab(as.character(input$pcB_sel)) + labs(color = legendname) +
          stat_ellipse()
      }else{
        #QC VISUALS PLOT
        values <- strsplit(input$tabs,"-")[[1]]
        req(length(values) == 2 )
        p <- ggplot(qcdata, aes(x=as.numeric(qcdata[,values[1]]), y=as.numeric(qcdata[,values[2]]), color=fill)) +
          geom_point(na.rm=TRUE) +
          labs(x=labels[values[1]],y=labels[values[2]], color=legendname) + 
          theme(legend.position="bottom",axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))
      }
      if(nlevels(fill) > 30){
        #p <- p + guides(color=FALSE)
      }
      
    }else if(main == "norm"){
      req(input$color_choice)
      req(input$meta)
      req(input$Attribute)
      req(input$tabs)
      if(input$color_choice){
        fill <- as.factor(ssheet[,input$meta])
        legendname <- input$meta
      }else{
        fill <- as.factor(qcdata[,input$Attribute])
        legendname <- input$Attribute
      }
      levels(fill) <- mixedsort(levels(fill))
      p <- ggplot(facs, aes(x=facs[,2], y= log10(facs[,1]), color=fill)) + geom_point(na.rm=TRUE) +
        labs(x="Negative Factor", y = "Positive Factor", color = legendname)
      
      if(input$tabs == "hf") {
        p <- ggplot(facs, aes(x=log10(facs[,1]), y=log10(facs[,3]), color=fill)) + geom_point(na.rm=TRUE) +
          labs(x="log10(Positive Factor)", y = "log10(Houskeeping factor)", color = legendname)
      }else if(input$tabs == "norm_res"){
        if(housekeep == ""){
          local_housekeep <- unique(control[control$CodeClass=="Housekeeping","Name"])
          print(local_housekeep)
        }else{
          local_housekeep <- unlist(strsplit(housekeep,"@"))
        }
        local_data <- control[control$Name %in% local_housekeep,]
        means <- qcdata["MC"]
        new <- data.frame("CodeClass" = rep("Average",nrow(means)),
                          "Name" = rep("Mean",nrow(means)),
                          "Accession" = rep("nacho",nrow(means)),
                          "Count" = means$MC,
                          "L1" = rownames(means))
        local_data <- rbind(local_data,new)
        local_data$positive <- sapply(local_data$L1, function(x) facs[x,"Positive_factor"])
        local_data$housekeeping <- sapply(local_data$L1, function(x) facs[x,"House_factor"])
        local_data$negative <- sapply(local_data$L1, function(x) facs[x,"Negative_factor"])
        local_norm <- local_data
        local_norm$Count <- (as.numeric(local_data$Count) - local_data$negative) * local_data$positive * local_data$housekeeping
        local_norm$Count[local_norm$Count <= 0] <- 0.1
        local_norm$Count <- round(local_norm$Count)
        local_norm$status <- rep("Normalized",nrow(local_norm))
        local_data$status <- rep("Raw", nrow(local_data))
        combined <- rbind(local_norm, local_data)
        p <- ggplot(combined, aes(x=as.numeric(as.factor(L1)),y=as.numeric(Count),colour=Name))+
          geom_line() + facet_grid(~status) + scale_y_log10() + labs(x="Sample index",y="counts")
        
        #p <- ggplot
      }
    
    }else{
      "FATAL ERROR"
    }
    if(main == "ot"){
    }else{
      p <- p + theme(axis.text=element_text(size=10),axis.title=element_text(size=14,face="bold"))# +
        #scale_color_manual(values=viridis)
      #p <- p + scale_colour_gradient(viridis_pal()(3))
      print(p)
    }

  })
}