#PTEE is an application deigned to help clinicians decide which is the most appropriate tissue to inquire gene expression given a specific phenotype of the patient.#
#PTEE uses GTEx expression data (mean TPMs of genes and transcripts)
#Version:
#Contact akhilvbioinfo@gmail.com or diana.leduc@gmail.com for further infrmation or support.
######################################################################################################################################################################

####################
#Load all libraries#
####################
library(shiny)
library(shinycssloaders)
library(shinythemes)
library(ggplot2)
library(shinyjs)
library(hrbrthemes)
library(ggcorrplot)
library(corrplot)
library(plotly)
library(reshape2)
library(tidyverse)
library(VennDiagram)
library(data.table)
library(vroom)
library(DT)
library(grid)
library(gridExtra)
library(nVennR)
library(shinydashboard)
#library(shinyalert)
#library(shinyWidgets)
######################################################################################################################################################################


###########################################################
#Function to select all tissues correspoding to a category#
###########################################################

onInitialize <- "
function(){
  var select = this.$input[0];
  this.$dropdown_content.on('mousedown', function(e){
    e.preventDefault(); 
    return false;
  }).on('click', '.optgroup-header', function(e){
    var options = $(this).parent().find('.option');
    var items = [];
    options.each(function(i, opt){items.push($(opt).data('value'));});
    var selections = select.selectize.items;
    select.selectize.setValue(items.concat(selections));
    
  });
  var select = this.$input[0];
  $('#reset').on('click', function(){
    select.selectize.setValue([]);
  });
}
"
######################################################################################################################################################################

#CHECK
#data3<-fread("data/data3.txt",sep = ",",header = T,check.names = FALSE)
#data4<-fread("data/data4.txt",sep = ",",header = T,check.names = FALSE)
#page3<-fread("data/page.txt",sep = ",",header = T,check.names = FALSE)
#data2<-fread("data/gtex_gene.txt",sep = ",",header = T,check.names = FALSE)
#gene_list<-fread("data/gene_list.txt")

##############################
#User interface startig point#
##############################

ui <-  fluidPage(

######################******Lgos and hyperlinks*****######################
  
  dbHeader <- dashboardHeader(
                                  tags$li(a(href = 'https://bioinf.eva.mpg.de/shiny/PTEE/',
                                  img(src = 'PTEE.png',
                                          title = "Phenotype-Tissue Expression & Exploration", height = "70px"),
                                      style = "padding-top:5px; padding-bottom:5px;"),
                                    class = "dropdown"),
                             ########################################
                             tags$li(a(href = 'https://www.uniklinikum-leipzig.de/einrichtungen/uzsel/',
                                       img(src = 'logo-uzsel-uniklinikum-leipzig.jpg',
                                           title = "Universitäres Zentrum für Seltene Erkrankungen Leipzig", height = "70px"),
                                       style = "padding-top:5px; padding-bottom:5px;"),
                                     class = "dropdown"),
                             ###########################################
                      
                              tags$li(a(href = 'https://www.uni-leipzig.de/en/',
                                        img(src = 'logo.svg',
                                            title = "Leipzig University", height = "70px"),
                                        style = "padding-top:5px; padding-bottom:5px;"),
                                      class = "dropdown"),
                          
                                      tags$li(a(href = 'https://www.eva.mpg.de/index.html',
                                                img(src = 'mpi.jpeg',
                                                    title = "Max Planck Institute for Evolutionary Anthropology", height = "70px"),
                                                style = "padding-top:5px; padding-bottom:5px;"),
                                      class = "dropdown"),
                            tags$li(a(href = 'https://biochemie.medizin.uni-leipzig.de/bch_cms/index.php/en/',
                                      img(src = 'logo_RSI.png',
                                          title = " Rudolf Schönheimer Institute of Biochemistry", height = "70px"),
                                      style = "padding-top:5px; padding-bottom:5px;"),
                                    class = "dropdown"),
                            tags$li(a(href = 'https://www.dfg.de/',
                                      img(src = 'DFG.jpg',
                                          title = "DFG - Deutsche Forschungsgemeinschaft", height = "70px"),
                                      style = "padding-top:5px; padding-bottom:5px;"),
                                    class = "dropdown")),
                       
                  
  useShinyjs(),
  #setBackgroundImage(src = "BG.png"),
  #shinyjs::extendShinyjs(text = "shinyjs.refresh = function() { location.reload(); }"),
  #includeCSS("styles.css"),
  #jsResetCode <- ("shinyjs.reset = function() {history.go(0)}"),
  titlePanel(""),
  
  #*****Page design*****
  
  title = "PTEE",
                theme = shinytheme("cerulean"),
                tags$head(tags$style(
                  HTML('
                       #sidebar {
                       background-color: #ffffff;
                       }
                       
                       body, label, input, button, select { 
                       font-family: "Arial";
                       }
                       .btn-file {
                        background-color:#5B81AE; 
                        border-color: #5B81AE; 
                        background: #5B81AE;
                      }
                      
         
                      .bttn-bordered.bttn-sm {
                          width: 0px;
                          text-align: left;
                          margin-bottom : 20px;
                          margin-top : 20px;
                       }'
                       
                  )
                )),
  #themeSelector(),
######################################################################################################################################################################


######################*****User selection for sidebar*****######################
  useShinyjs(),
  sidebarLayout(
    sidebarPanel(width = 4,
      conditionalPanel(
        'input.dataset === "Choose Phenotype"',
        selectInput("Gene_list", "Select Your Phenotype of Interest",
                    list(
                      "NDD Genes from SysID" = "ndd",
                      "Cardiac Arrythmia" = "ca","Obesity" = "ob",
                     `Phenomizer Genes` = list("Abdomen Abnormality	HP:0001438"="abd-abn",
                                                                           "Blood Abnormality HP:0001871"="blo-abn",
                                                                           "Breast Abnormality HP:0000769"="bre-abn",
                                                                           "Cardiovascular Abnormality HP:0001626"="car-abn",
                                                                           "Connective Tissue Abnormality HP:0003549"="con-t-ab",
                                                                           "Ear Abbnormality HP:0000598"="ear-ab",
                                                                           "Endocrine Abnormality HP:0000818"="end-abn",
                                                                           "Eye Abnormality HP:0000478"="eye-abn",
                                                                           "Genitourinary Abnormality HP:0000119"="gen-abn",
                                                                           "Growth Abnormality HP:0001507"="gro-abn",
                                                                           "Head Neck Abnormality HP:0000152"="hea-n-abn",
                                                                           "Immune Abnormality HP:0002715"="imm-abn",
                                                                           "Integument Abbnormality HP:0001574"="int-abn",
                                                                           "Limbs Abnormality HP:0040064"="lim-abn",
                                                                           "Metabbolism Abnormality HP:0001939"="met-abn",
                                                                           "Musculature Abnormality HP:0003011"="mus-abn",
                                                                           "Neoplasm HP:0002664"="neo",
                                                                           "Nervous System Abnormality HP:0000707"="ner-s-abn",
                                                                           "Prenatal Abnormality HP:0001197"="pre-abn",
                                                                           "Respiratory Abnormality HP:0002086"="res-abn",
                                                                           "Skeletal Abnormality HP:0000924"="ske-abn",
                                                                           "Thoracic Abnormality HP:0045027"="tho-abn",
                                                                           "Voice Abnormality HP:0001608"="voi-abn")),selected = c("tho-abn")),
        selectizeInput("show_vars5", "Tissue of Interest:",
                       list(`Adipose tissue` = list("Adipose Subcutaneous", "Adipose Visceral (Omentum)"),
                            `Endocrine Glands` = list("Adrenal Gland", "Pituitary", "Thyroid","Pancreas"),
                            `Arteries` = list("Artery Aorta", "Artery Coronary", "Artery Tibial"),
                            `Genito urinary tract` = list("Bladder", "Ovary", "Testis","Prostate","Fallopian Tube","Cervix Ectocervix","Cervix Endocervix","Uterus",
                                                           "Vagina","Kidney Cortex","Kidney Medulla"),
                            `Cells` = list("Cells EBV transformed lymphocytes","Cells Cultured fibroblasts"),
                            `Central nervous system` = list("Brain Amygdala", "Brain Anterior cingulate cortex (BA24)", "Brain Caudate (basal ganglia)","Brain Cerebellar Hemisphere",
                                                            "Brain Cerebellum","Brain Cortex","Brain Frontal Cortex (BA9)","Brain Hippocampus","Brain Hypothalamus","Brain Nucleus accumbens (basal ganglia)",
                                                            "Brain Putamen (basal ganglia)","Brain Spinal cord (cervical c-1)","Brain Substantia nigra"),
                            `Breast - Mammary Tissue` = list("Breast Mammary Tissue"),
                            #`Cells` = list("Cells Cultured fibroblasts"),
                            `Digestive tract` = list("Colon Sigmoid", "Colon Transverse", "Esophagus Gastroesophageal Junction","Esophagus Mucosa",
                                                     "Esophagus Muscularis","Minor Salivary Gland","Pancreas","Stomach","Liver","Small Intestine Terminal Ileum"),
                            `Heart` = list("Heart Atrial Appendage", "Heart Left Ventricle"),
                            `Lung` = list("Lung"),
                            `Muscle – Skeletal` = list("Muscle Skeletal"),
                            `Nerve – Tibial` = list("Nerve Tibial"),`Skin` = list("Skin Not Sun Exposed Suprapubic", "Skin Sun Exposed Lower leg"),
                            `Spleen` = list("Spleen"),`Whole Blood` = list("Whole Blood")),
                       multiple = TRUE,selected = c("Brain Cortex"),
                       options = list(
                         onInitialize = I(onInitialize)
                       )
        ),
        selectizeInput("show_vars2", "Accessible Tissue:",
                       list(`Cells` = list("Cells EBV transformed lymphocytes","Cells Cultured fibroblasts"),
                            `Skin` = list("Skin Not Sun Exposed Suprapubic", "Skin Sun Exposed Lower leg"),`Muscle – Skeletal` = list("Muscle Skeletal"),
                            `Whole Blood` = list("Whole Blood")),
                       multiple = TRUE,selected = c('Whole Blood',"Muscle Skeletal"),
                       options = list(
                         onInitialize = I(onInitialize)
                       )
        ),
        actionButton("reset", "Reset"),downloadLink("downloadData", "Manual")
        ,tags$hr(),
        checkboxInput(inputId = "Adv",label = strong("Advanced"),value = FALSE,width ='100%'),
        fileInput("file1", "Choose Gene List File (Ensembl Gene Id Or HGNC Symbol)",multiple = FALSE,accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
        checkboxInput(inputId = "pas",label = strong("OR"),value = FALSE,width ='100%'),
        textAreaInput("paste", "Paste Gene ID",placeholder ="Paste comma or space separated Ensembl Gene Id Or HGNC Symbol"),
        
        #extendShinyjs(text = jsResetCode),
        actionButton('reset02', 'Reset'),
        downloadLink('example', 'Download example data',class = "butt"), tags$head(tags$style(".butt{background-color} .butt{color: #bf7136;}")),
      ),
      
######################################################################################################################################################################


######################*****Side panel design*****######################

      conditionalPanel(
        'input.dataset === "Correlation Analysis"',
        #helpText("Tissue of Interest"),
        #selectInput("show_vars5", 'Tissue of Interest',gsub('\\.', ' ', names(data4)), multiple=TRUE, selectize=TRUE,selected = c('Adipose Subcutaneous','Bladder')),  
        #selectInput("show_vars2", 'Accessible Tissue',gsub('\\.', ' ', names(data3)), multiple=TRUE, selectize=TRUE,selected = 'Whole Blood'),
        selectizeInput("show_vars32", "Tissue of Interest (TI)",
                       list(`Adipose tissue` = list("Adipose Subcutaneous", "Adipose Visceral (Omentum)"),
                            `Endocrine Glands` = list("Adrenal Gland", "Pituitary", "Thyroid","Pancreas"),
                            `Arteries` = list("Artery Aorta", "Artery Coronary", "Artery Tibial"),
                            `Genito urinary tract` = list("Bladder", "Ovary", "Testis","Prostate","Fallopian Tube","Cervix Ectocervix","Cervix Endocervix","Uterus",
                                                          "Vagina","Kidney Cortex","Kidney Medulla"),
                            `Central nervous system` = list("Brain Amygdala", "Brain Anterior cingulate cortex (BA24)", "Brain Caudate (basal ganglia)","Brain Cerebellar Hemisphere",
                                                            "Brain Cerebellum","Brain Cortex","Brain Frontal Cortex (BA9)","Brain Hippocampus","Brain Hypothalamus","Brain Nucleus accumbens (basal ganglia)",
                                                            "Brain Putamen (basal ganglia)","Brain Spinal cord (cervical c-1)","Brain Substantia nigra"),
                            `Breast - Mammary Tissue` = list("Breast Mammary Tissue"),
                            #`Cells` = list("Cells Cultured fibroblasts"),
                            `Digestive tract` = list("Colon Sigmoid", "Colon Transverse", "Esophagus Gastroesophageal Junction","Esophagus Mucosa",
                                                     "Esophagus Muscularis","Minor Salivary Gland","Pancreas","Stomach","Liver","Small Intestine Terminal Ileum"),
                            `Heart` = list("Heart Atrial Appendage", "Heart Left Ventricle"),
                            `Lung` = list("Lung"),
                            `Muscle – Skeletal` = list("Muscle Skeletal"),
                            `Nerve – Tibial` = list("Nerve Tibial"),
                            `Spleen` = list("Spleen"),`Cells` = list("Cells EBV transformed lymphocytes","Cells Cultured fibroblasts"),
                            `Skin` = list("Skin Not Sun Exposed Suprapubic", "Skin Sun Exposed Lower leg"),
                            `Whole Blood` = list("Whole Blood")),
                       multiple = T,selected = c("Brain Cortex"),
                       options = list(onInitialize = I(onInitialize)
                         
                       )
        ),
        selectizeInput("show_vars33", "Accessible Tissue (AT)",
                       list(`Cells` = list("Cells EBV transformed lymphocytes","Cells Cultured fibroblasts"),
                            `Skin` = list("Skin Not Sun Exposed Suprapubic", "Skin Sun Exposed Lower leg"),
                            `Whole Blood` = list("Whole Blood"),`Muscle – Skeletal` = list("Muscle Skeletal")),
                       multiple = TRUE,selected = c('Whole Blood',"Muscle Skeletal"),
                       options = list(onInitialize = I(onInitialize)
                         
                       )
        ),
        radioButtons(inputId = "var10", label = "Select file type for the plot", choices = list("pdf", "png"))

      ),
      conditionalPanel(
        'input.dataset === "Expression Analysis"',
        selectizeInput("show_vars22", "Tissue of Interest (TI)",
                       list(`Adipose tissue` = list("Adipose Subcutaneous", "Adipose Visceral (Omentum)"),
                            `Endocrine Glands` = list("Adrenal Gland", "Pituitary", "Thyroid","Pancreas"),
                            `Arteries` = list("Artery Aorta", "Artery Coronary", "Artery Tibial"),
                            `Genito urinary tract` = list("Bladder", "Ovary", "Testis","Prostate","Fallopian Tube","Cervix Ectocervix","Cervix Endocervix","Uterus",
                                                           "Vagina","Kidney Cortex","Kidney Medulla"),
                            `Central nervous system` = list("Brain Amygdala", "Brain Anterior cingulate cortex (BA24)", "Brain Caudate (basal ganglia)","Brain Cerebellar Hemisphere",
                                                            "Brain Cerebellum","Brain Cortex","Brain Frontal Cortex (BA9)","Brain Hippocampus","Brain Hypothalamus","Brain Nucleus accumbens (basal ganglia)",
                                                            "Brain Putamen (basal ganglia)","Brain Spinal cord (cervical c-1)","Brain Substantia nigra"),
                            `Breast - Mammary Tissue` = list("Breast Mammary Tissue"),
                            #`Cells` = list("Cells Cultured fibroblasts"),
                            `Digestive tract` = list("Colon Sigmoid", "Colon Transverse", "Esophagus Gastroesophageal Junction","Esophagus Mucosa",
                                                     "Esophagus Muscularis","Minor Salivary Gland","Pancreas","Stomach","Liver","Small Intestine Terminal Ileum"),
                            `Heart` = list("Heart Atrial Appendage", "Heart Left Ventricle"),
                            `Lung` = list("Lung"),
                            `Muscle – Skeletal` = list("Muscle Skeletal"),
                            `Nerve – Tibial` = list("Nerve Tibial"),
                            `Spleen` = list("Spleen"),`Cells` = list("Cells EBV transformed lymphocytes","Cells Cultured fibroblasts"),
                            `Skin` = list("Skin Not Sun Exposed Suprapubic", "Skin Sun Exposed Lower leg"),
                            `Whole Blood` = list("Whole Blood")),
                       multiple = T,selected = c('Adipose Subcutaneous'),
                       options = list(onInitialize = I(onInitialize)
                         
                       )
        ),
        selectizeInput("show_vars23", "Accessible Tissue (AT)",
                       list(`Cells` = list("Cells EBV transformed lymphocytes","Cells Cultured fibroblasts"),
                            `Skin` = list("Skin Not Sun Exposed Suprapubic", "Skin Sun Exposed Lower leg"),
                            `Whole Blood` = list("Whole Blood"),`Muscle – Skeletal` = list("Muscle Skeletal")),
                       multiple = TRUE,selected = 'Whole Blood',
                       options = list(onInitialize = I(onInitialize)
                         
                       )
        ),
        helpText(strong("Venn Diagram of Expresed Genes"),br(),"AT = Accessible Tissue",br(),"TI = Tissue of Interest",
                 br(),"PI = Phenotype of Interest",br(),"Note that for Advanced option, PI will consist of all genes")
      ),
      conditionalPanel(
        'input.dataset === "Single Gene Analysis"',
        #helpText("Enter the gene name"),
        selectizeInput(inputId = 'mylist', label = 'Enter Gene ID',choices = NULL,selected = NULL,multiple=TRUE),
        
        #helpText("Enter the tissue name"),
        selectizeInput("show_vars4", 'Select Tissue', list(`Adipose tissue` = list("Adipose Subcutaneous", "Adipose Visceral (Omentum)"),
                                                                `Endocrine Glands` = list("Adrenal Gland", "Pituitary", "Thyroid","Pancreas"),
                                                                `Arteries` = list("Artery Aorta", "Artery Coronary", "Artery Tibial"),
                                                                `Genito urinary tract` = list("Bladder", "Ovary", "Testis","Prostate","Fallopian Tube","Cervix Ectocervix","Cervix Endocervix","Uterus",
                                                                                               "Vagina","Kidney Cortex","Kidney Medulla"),
                                                                `Central nervous system` = list("Brain Amygdala", "Brain Anterior cingulate cortex (BA24)", "Brain Caudate (basal ganglia)","Brain Cerebellar Hemisphere",
                                                                                                "Brain Cerebellum","Brain Cortex","Brain Frontal Cortex (BA9)","Brain Hippocampus","Brain Hypothalamus","Brain Nucleus accumbens (basal ganglia)",
                                                                                                "Brain Putamen (basal ganglia)","Brain Spinal cord (cervical c-1)","Brain Substantia nigra"),
                                                                `Breast - Mammary Tissue` = list("Breast Mammary Tissue"),
                                                                #`Cells` = list("Cells Cultured fibroblasts"),
                                                                `Digestive tract` = list("Colon Sigmoid", "Colon Transverse", "Esophagus Gastroesophageal Junction","Esophagus Mucosa",
                                                                                         "Esophagus Muscularis","Minor Salivary Gland","Pancreas","Stomach","Liver","Small Intestine Terminal Ileum"),
                                                                `Heart` = list("Heart Atrial Appendage", "Heart Left Ventricle"),
                                                                `Lung` = list("Lung"),
                                                                `Muscle – Skeletal` = list("Muscle Skeletal"),
                                                                `Nerve – Tibial` = list("Nerve Tibial"),`Cells` = list("Cells EBV transformed lymphocytes","Cells Cultured fibroblasts"),
                                                                `Skin` = list("Skin Not Sun Exposed Suprapubic", "Skin Sun Exposed Lower leg"),
                                                                `Whole Blood` = list("Whole Blood")), multiple=TRUE,options = list(onInitialize = I(onInitialize))),
        #helpText("Visualize the TPM values of selected genes")
        
      ),
      #conditionalPanel( 'input.dataset === "Conclusion"', helpText("The results of selected phenotypes")),
      conditionalPanel(
        'input.dataset === "How to Use"',
        helpText("Instructions for using this page")
      ),
      conditionalPanel(
        'input.dataset === "Transcript Analysis"',
        #helpText("Transcript Analysis of selected Phenotypes"),
        selectizeInput("show_vars15", "Tissue of Interest (TI)",
                       list(`Adipose tissue` = list("Adipose Subcutaneous", "Adipose Visceral (Omentum)"),
                            `Endocrine Glands` = list("Adrenal Gland", "Pituitary", "Thyroid","Pancreas"),
                            `Arteries` = list("Artery Aorta", "Artery Coronary", "Artery Tibial"),
                            `Genito urinary tract` = list("Bladder", "Ovary", "Testis","Prostate","Fallopian Tube","Cervix Ectocervix","Cervix Endocervix","Uterus",
                                                           "Vagina","Kidney Cortex","Kidney Medulla"),
                            `Central nervous system` = list("Brain Amygdala", "Brain Anterior cingulate cortex (BA24)", "Brain Caudate (basal ganglia)","Brain Cerebellar Hemisphere",
                                                            "Brain Cerebellum","Brain Cortex","Brain Frontal Cortex (BA9)","Brain Hippocampus","Brain Hypothalamus","Brain Nucleus accumbens (basal ganglia)",
                                                            "Brain Putamen (basal ganglia)","Brain Spinal cord (cervical c-1)","Brain Substantia nigra"),
                            `Breast - Mammary Tissue` = list("Breast Mammary Tissue"),
                            #`Cells` = list("Cells Cultured fibroblasts"),
                            `Digestive tract` = list("Colon Sigmoid", "Colon Transverse", "Esophagus Gastroesophageal Junction","Esophagus Mucosa",
                                                     "Esophagus Muscularis","Minor Salivary Gland","Pancreas","Stomach","Liver","Small Intestine Terminal Ileum"),
                            `Heart` = list("Heart Atrial Appendage", "Heart Left Ventricle"),
                            `Lung` = list("Lung"),
                            `Muscle – Skeletal` = list("Muscle Skeletal"),
                            `Nerve – Tibial` = list("Nerve Tibial"),
                            `Spleen` = list("Spleen"),`Cells` = list("Cells EBV transformed lymphocytes","Cells Cultured fibroblasts"),
                            `Skin` = list("Skin Not Sun Exposed Suprapubic", "Skin Sun Exposed Lower leg"),
                            `Whole Blood` = list("Whole Blood")),
                       multiple = T,selected = c('Adipose Subcutaneous'),
                       options = list(onInitialize = I(onInitialize)
                         
                       )
        ),
        selectizeInput("show_vars12", "Accessible Tissue (AT)",
                       list(`Cells` = list("Cells EBV transformed lymphocytes","Cells Cultured fibroblasts"),
                            `Skin` = list("Skin Not Sun Exposed Suprapubic", "Skin Sun Exposed Lower leg"),
                            `Whole Blood` = list("Whole Blood"),`Muscle – Skeletal` = list("Muscle Skeletal")),
                       multiple = TRUE,selected = c('Whole Blood'),
                       options = list(onInitialize = I(onInitialize)
                        
                       )
        ),
        #selectInput("show_vars15", 'Tissue of Interest (TI)',gsub('\\.', ' ', names(data4)), multiple= FALSE, selectize=TRUE,selected = 'Adipose.Subcutaneous'),  
        #selectInput("show_vars12", 'Accessible Tissue (AT)',gsub('\\.', ' ', names(data3)), multiple=TRUE, selectize=TRUE,),
      )
    ),
######################################################################################################################################################################


#####################*****Main panel Design*****#####################
mainPanel(
      tabsetPanel(
        id = 'dataset',
        tabPanel("Choose Phenotype",bordered = F,imageOutput("myImage")%>% withSpinner(color="#0dc5c1"),htmlOutput("not_list"),DT::dataTableOutput("table1")%>% withSpinner(color="#0dc5c1"),downloadButton(outputId = "mydownload123",label = "Download Table")),
        tabPanel("Correlation Analysis",DT::dataTableOutput("mytable2"),downloadButton(outputId = "mydownload2",label = "Download Table"),
                 plotOutput("plot1", width = 800, height = 600)%>% withSpinner(color="#0dc5c1"),downloadButton(outputId = "down2", label = "Download the plot"),
                 plotOutput("plot5", width = 800, height = 600),downloadButton(outputId = "down3", label = "Download the plot")),
        tabPanel("Expression Analysis",bordered = F,imageOutput("myImagevenn", width = 800, height = 500)%>% withSpinner(color="#0dc5c1"),DT::dataTableOutput("mytable20"),downloadButton(outputId = "mydownload20",label = "Download Table")),
        tabPanel("Transcript Analysis",imageOutput("Vennout", width = 800, height = 500)%>% withSpinner(color="#0dc5c1"),DT::dataTableOutput("mytable12"),downloadButton(outputId = "mydownload05",label = "Download Table")),
        tabPanel("Single Gene Analysis",textOutput("selected_var"),plotOutput("plot7", width = 1000, height = 600)%>% withSpinner(color="#0dc5c1"),textOutput("selected_var2"),DT::dataTableOutput("table05")%>% withSpinner(color="#0dc5c1"),plotOutput("plot8", width = 1000, height = 600), brush = "user_brush")
        ,
        #tabPanel(title = "Conclusion", strong("The results as follws"),p("Based on the GE analysis the best Accessible tissue for the XXXXX and phenotype XXXXXXXXX")),
        tabPanel(
          title = "About",
          strong("Description"),
          p("Phenotype-Tissue Expression & Exploration is a web based tool designed to help clinical geneticists decide which accessible tissue is best suited for RNA-seq analysis. Based on the human phenotype ontology (HPO) annotation the clinician can choose which phenotypical abnormality best matches the patient. This is linked to a tissue of interest (e.g. heart for HP:0001626 abnormality of the cardiovascular system) that can further be selected by the clinician. In the subsequent analyses only genes that are annotated to the chosen HPO will be included. The clinician can then choose between four usually accessible tissues (whole blood, sun exposed skin, not sun exposed skin, and cultured lymphocytes – Epstein-Barr virus (EBV) transformation)."),
          strong("Correlation Analysis"),
          p("This analysis shows the degree of correlation based on genes expressed (median Transcripts Per Kilobase Million (TPM) > 1.5) in the clinically accessible tissue vs. tissue of interest for genes annotated in the selected HPO."),
          strong("Transcript Analysis"),
          p("This analysis shows the degree of intersection between transcripts expressed (median TPM > 1.5) in the clinically accessible tissue vs. tissue of interest for genes annotated in the selected HPO. For a chosen gene you can visualize which transcripts are expressed in each tissue."),
          strong("Single Gene Analysis"),
          p("For a chosen gene you can visualize the TPM values in the tissue of interest and the selected accessible tissues."),
          #strong("Conclusion"),p("On this page we will infer which accessible tissue best matches expression data of the tissue of interest based on the chosen phenotype."),
           br(),
            p(em("This tool is designed on R-Shiny package and the data from Genotype-Tissue Expression (GTEx) project (https://gtexportal.org/home/)")),
          icon = icon("question-circle"))

      )
    )
  )
)

##############################
#End of user interface design#
##############################


######################################################################################################################################################################


################################
#Server function starting point#
################################
server <- function(input, output,session) 
{

  observe({

#####################*****Advanced Option Functions*****#####################
        
    if((input$Adv == TRUE)) {
      shinyjs::show(id = "file1")
      shinyjs::show(id = "reset02")
      #shinyjs::show(id = "downloadData")
      shinyjs::show(id = "example")
      shinyjs::show(id = "paste")
      shinyjs::show(id = "pas")
      shinyjs::disable("Gene_list")
      shinyjs::disable("mylist")
      shinyjs::enable("file1")
      output$myImage <-renderImage({
        Leg<-"image/All_genes.png" 
        list(src=Leg)},deleteFile = FALSE)
      
    } else {
      
      shinyjs::hide(id = "file1")
      shinyjs::hide(id = "reset02")
      #shinyjs::hide(id = "downloadData")
      shinyjs::hide(id = "example")
      shinyjs::hide(id = "paste")
      shinyjs::hide(id = "pas")
      shinyjs::enable("Gene_list")
      shinyjs::enable("mylist")
      shinyjs::disable("file1")
      reset("pas")
    }
  })
  
  observe({
    
    if((input$pas == TRUE)) {
      reset("file1")
      shinyjs::disable("file1")
      shinyjs::enable("paste")
      rv$df<-NULL
      
    }
    else {
      shinyjs::disable("paste")
      reset("paste")
      shinyjs::enable("file1")
    }
  })
  
  
######################################################################################################################################################################

  output$downloadData <- downloadHandler(
    filename = "PTEE_Manual.pdf",
    content = function(file) {
      file.copy("www/manual.pdf", file)
    }
  )
  output$example <- downloadHandler(
    filename = "Example_gene_list.txt",
    content = function(file) {
      file.copy("www/Example_gene_list.txt", file)
    }
  )
######################################################################################################################################################################

  observeEvent(input$reset1, {
    reset("show_vars5")})
  observeEvent(input$reset2, {
    reset("show_vars2")})
######################################################################################################################################################################

############******Read Gene TPMs*****####################
dataa<-reactive({
  if((input$Gene_list == 'ca')) 
    { fread('data/Cardiac-Arrythmia.txt',sep = "\t",header = T,check.names = FALSE)
  }
  else{
  if((input$Adv == TRUE)) 
  { fread('data/all.txt',sep = "\t",header = T,check.names = FALSE)
  }
    else{
      if (input$Gene_list == 'ob')
      {
        fread('data/Obesity.txt',sep = "\t",header = T,check.names = FALSE)
      }
    else{
  if (input$Gene_list == 'ndd')
  {
    fread('data/sysID.txt',sep = ",",header = T,check.names = FALSE)
  }
  else{
  if (input$Gene_list == 'blo-abn')
    {
    fread('data/Blood_abnormality.txt',sep = "\t",header = T,check.names = FALSE)
  }
    else{
      if (input$Gene_list == 'bre-abn')
      {
        fread('data/Breast_abnormality.txt',sep = ",",header = T,check.names = FALSE)
      }
      else{
        if (input$Gene_list == 'abd-abn')
        {
          fread('data/Abdomen-abnormality.txt',sep = ",",header = T,check.names = FALSE)
        }
        else{
          if (input$Gene_list == 'car-abn')
          {
            fread('data/Cardiovascular-abnormality.txt',sep = ",",header = T,check.names = FALSE)
          }
          else{
            if (input$Gene_list == 'con-t-ab')
            {
              fread('data/Connective_tissue_abnormality.txt',sep = ",",header = T,check.names = FALSE)
            }
            else{
              if (input$Gene_list == 'ear-ab')
              {
                fread('data/Ear-abbnormality.txt',sep = ",",header = T,check.names = FALSE)
              }
              else{
                if (input$Gene_list == 'end-abn')
                {
                  fread('data/Endocrine-abnormality.txt',sep = ",",header = T,check.names = FALSE)
                }
                else{
                  if (input$Gene_list == 'eye-abn')
                  {
                    fread('data/Eye-abnormality.txt',sep = ",",header = T,check.names = FALSE)
                  }
                  else{
                    if (input$Gene_list == 'gen-abn')
                    {
                      fread('data/Genitourinary_abnormality.txt',sep = ",",header = T,check.names = FALSE)
                    }
                    else{
                      if (input$Gene_list == 'gro-abn')
                      {
                        fread('data/Growth-abnormality.txt',sep = ",",header = T,check.names = FALSE)
                      }
                      else{
                        if (input$Gene_list == 'hea-n-abn')
                        {
                          fread('data/Head-neck-abnormality.txt',sep = ",",header = T,check.names = FALSE)
                        }
                        else{
                          if (input$Gene_list == 'imm-abn')
                          {
                            fread('data/Immune-abnormality.txt',sep = ",",header = T,check.names = FALSE)
                          }
                          else{
                            if (input$Gene_list == 'int-abn')
                            {
                              fread('data/Integument-abbnormality.txt',sep = ",",header = T,check.names = FALSE)
                            }
                          else{
                            if (input$Gene_list == 'lim-abn')
                            {
                              fread('data/Limbs_abnormality.txt',sep = ",",header = T,check.names = FALSE)
                            }
                            else{
                              if (input$Gene_list == 'met-abn')
                              {
                                fread('data/Metabbolism_abnormality.txt',sep = ",",header = T,check.names = FALSE)
                              }
                              else{
                                if (input$Gene_list == 'mus-abn')
                                {
                                  fread('data/Musculature-abnormality.txt',sep = ",",header = T,check.names = FALSE)
                                }
                                else{
                                  if (input$Gene_list == 'neo')
                                  {
                                    fread('data/Neoplasm.txt',sep = ",",header = T,check.names = FALSE)
                                  }
                                  else{
                                    if (input$Gene_list == 'ner-s-abn')
                                    {
                                      fread('data/Nervous-system-abnormality.txt',sep = ",",header = T,check.names = FALSE)
                                    }
                                    else{
                                      if (input$Gene_list == 'pre-abn')
                                      {
                                        fread('data/Prenatal-abnormality.txt',sep = ",",header = T,check.names = FALSE)
                                      }
                                      else{
                                        if (input$Gene_list == 'res-abn')
                                        {
                                          fread('data/Respiratory_abnormality.txt',sep = ",",header = T,check.names = FALSE)
                                        }
                                        else{
                                          if (input$Gene_list == 'ske-abn')
                                          {
                                            fread('data/Skeletal-abnormality.txt',sep = ",",header = T,check.names = FALSE)
                                          }
                                          else{
                                            if (input$Gene_list == 'tho-abn')
                                            {
                                              fread('data/Thoracic-abnormality.txt',sep = ",",header = T,check.names = FALSE)
                                            }
                                            else{
                                              if (input$Gene_list == 'voi-abn')
                                              {
                                                fread('data/Voice-abnormality.txt',sep = ",",header = T,check.names = FALSE)
                                              }
                                              else{
                                                if (input$Gene_list == 'all')
                                                {
                                                  fread('data/all.txt',sep = "\t",header = T,check.names = FALSE)
                                                }
                                     }}}}}}}}}}}}}}}}}}}}}}}}}}}
  
})

######################################################################################################################################################################

tpm_data<-reactive({
  if((input$Gene_list == 'ca'))
  { 
    vroom("tpm/Cardiac-Arrythmia.zip")
  }
  else{
    if (input$Gene_list == 'ndd')
    {
      vroom("tpm/sysID.zip")
    }
    else{
      if (input$Gene_list == 'ob')
      {
        vroom("tpm/Obesity.zip")
      }
    else{
      if (input$Gene_list == 'blo-abn')
      {
        vroom("tpm/Blood_abnormality-HP_0001871.zip")
      }
      else{
        if (input$Gene_list == 'bre-abn')
        {
          vroom("tpm/Breast_abnormality_HP_0000769.zip")
        }
        else{
          if (input$Gene_list == 'abd-abn')
          {
            vroom("tpm/Abdomen-abnormality_HP_0001438.zip")
  }
  else{
    if (input$Gene_list == 'car-abn')
    {
      vroom("tpm/Cardiovascular-abnormality_HP_0001626.zip")
    }
    else{
      if (input$Gene_list == 'con-t-ab')
      {
        vroom("tpm/Connective_tissue_abnormality-HP_0003549.zip")
      }
      else{
        if (input$Gene_list == 'ear-ab')
        {
          vroom("tpm/Ear-abbnormality_HP_0000598.zip")
        }
        else{
          if (input$Gene_list == 'end-abn')
          {
            vroom("tpm/Endocrine-abnormality_HP_0000818.zip")
          }
          else{
            if (input$Gene_list == 'eye-abn')
            {
              vroom("tpm/Eye-abnormality_HP_0000478.zip")
            }
            else{
              if (input$Gene_list == 'gen-abn')
              {
                vroom("tpm/Genitourinary_abnormality_HP_0000119.zip")
              }
              else{
                if (input$Gene_list == 'gro-abn')
                {
                  vroom("tpm/Growth-abnormality_HP_0001507.zip")
                }
                else{
                  if (input$Gene_list == 'hea-n-abn')
                  {
                    vroom("tpm/Head-neck-abnormality_HP_0000152.zip")
                  }
                  else{
                    if (input$Gene_list == 'imm-abn')
                    {
                      vroom("tpm/Immune-abnormality_HP_0002715.zip")
                    }
                    else{
                      if (input$Gene_list == 'int-abn')
                      {
                        vroom("tpm/Integument-abbnormality_HP_0001574.zip")
                      }
                      else{
                        if (input$Gene_list == 'lim-abn')
                        {
                          vroom("tpm/Limbs_abnormality_HP_0040064.zip")
                        }
                        else{
                          if (input$Gene_list == 'met-abn')
                          {
                            vroom("tpm/Metabbolism_abnormality_HP_0001939.zip")
                          }
                          else{
                            if (input$Gene_list == 'mus-abn')
                            {
                              vroom("tpm/Musculature-abnormality_HP_0003011.zip")
                            }
                            else{
                              if (input$Gene_list == 'neo')
                              {
                                vroom("tpm/Neoplasm_HP_0002664.zip")
                              }
                              else{
                                if (input$Gene_list == 'ner-s-abn')
                                {
                                  vroom("tpm/Nervous-system-abnormality_HP0000707.zip")
                                }
                                else{
                                  if (input$Gene_list == 'pre-abn')
                                  {
                                    vroom("tpm/Prenatal-abnormality_HP_0001197.zip")
                                  }
                                  else{
                                    if (input$Gene_list == 'res-abn')
                                    {
                                      vroom("tpm/Respiratory_abnormality_HP_0002086.zip")
                                    }
                                    else{
                                      if (input$Gene_list == 'ske-abn')
                                      {
                                        vroom("tpm/Skeletal-abnormality_HP_0000924.zip")
                                      }
                                      else{
                                        if (input$Gene_list == 'tho-abn')
                                        {
                                          vroom("tpm/Thoracic-abnormality_HP_0045027.zip")
                                        }
                                        else{
                                          if (input$Gene_list == 'voi-abn')
                                          {
                                            vroom("tpm/Voice-abnormality_HP_0001608.zip")
                                          }
                                        }}}}}}}}}}}}}}}}}}}}}}}}}

})
  
######################################################################################################################################################################

#Image on the first page
######################################################################################
output$myImage <-renderImage({
  if (input$Gene_list == 'ca') Leg<-"image/Cardiac_Arrhythmia.png"
  if (input$Gene_list == 'blo-abn') Leg<-"image/Blood_abnormality.png"
  if (input$Gene_list == 'bre-abn') Leg<-"image/Breast_abnormality.png"
  if (input$Gene_list == 'abd-abn') Leg<-"image/Abdomen_abnormality.png"
  if (input$Gene_list == 'car-abn') Leg<-"image/Cardiovascular_abnormality.png"
  if (input$Gene_list == 'con-t-ab') Leg<-"image/Connective_tissue_abnormality.png"
  if (input$Gene_list == 'ear-ab') Leg<-"image/Ear_abnormality.png"
  if (input$Gene_list == 'end-abn') Leg<-"image/Endocrine_abnormality.png"
  if (input$Gene_list == 'eye-abn') Leg<-"image/Eye_abnormality.png"
  if (input$Gene_list == 'gen-abn') Leg<-"image/Genitourinary_abnormality.png"
  if (input$Gene_list == 'gro-abn') Leg<-"image/Growth_abnormality.png"
  if (input$Gene_list == 'hea-n-abn') Leg<-"image/Head_neck_abnormality.png"
  if (input$Gene_list == 'imm-abn') Leg<-"image/Immune_abnormality.png"
  if (input$Gene_list == 'int-abn') Leg<-"image/Integument_abnormality.png"
  if (input$Gene_list == 'lim-abn') Leg<-"image/Limbs_abnormality.png"
  if (input$Gene_list == 'met-abn') Leg<-"image/Metabolism_abnormality.png"
  if (input$Gene_list == 'mus-abn') Leg<-"image/Musculature_abnormality.png"
  if (input$Gene_list == 'neo') Leg<-"image/Neoplasm.png"
  if (input$Gene_list == 'ner-s-abn') Leg<-"image/Nervous_system_abnormality.png"
  if (input$Gene_list == 'pre-abn') Leg<-"image/Prenatal_abnormality.png"
  if (input$Gene_list == 'res-abn') Leg<-"image/Respiratory_abnormality.png"
  if (input$Gene_list == 'ske-abn') Leg<-"image/Skeletal_abnormality.png"
  if (input$Gene_list == 'tho-abn') Leg<-"image/Thoracic_abnormality.png"
  if (input$Gene_list == 'voi-abn') Leg<-"image/Voice_abnormality.png"
  if (input$Gene_list == 'ndd') Leg<-"image/NDD.png"
  if (input$Gene_list == 'ob') Leg<-"image/Obesity.png"
 
  list(src=Leg,width = "625", height = "437.5",style="position:absolute;right:300px;z-index:1000000;")
},deleteFile = FALSE)
######################################################################################################################################################################

#******Page one**********####################
 

 #observeEvent(input$reset02, {
 # shinyjs::js$refresh()
 # })
  #observeEvent(input$reset02, {js$reset()})  
############################Reading of user uploaded file####################
  rv <- reactiveValues(df =NULL)
  observe({
    req(input$file1)
    rv$df <- read.csv(input$file1$datapath,header = F)
  })
  
  observeEvent(input$reset02, {
    reset('pas')
    reset('paste')
    rv$df<-NULL
    reset('file1')
  })
  
 
########################################
  
output$table1 <-DT::renderDataTable({
  shiny::validate(need(input$show_vars2,"Enter one Accessible Tissue"))
  shiny::validate(need(input$show_vars5,"Enter one Tissue of Interest"))
######################################

  if (input$Adv == FALSE&&input$pas == FALSE)
    {
    
    data <- dataa()
    data[, 3:ncol(data)] <- round(data[, 3:ncol(data)], 2)
    data<-(as.matrix(data))
    list<-c("Gene ID","Ensembl ID",input$show_vars2,input$show_vars5)
    table<-(data[,list])
    output$mydownload123=downloadHandler(filename = "Phenotype of Interest Table",content = function(file){write.csv((data[,list]),file)})
    datatable(table,class = 'cell-border stripe',
                  caption = HTML("Data Source: GTEx Analysis Release V8 <br/>
Displayed numbers represent median gene-level TPM by tissue"),)
  }
  else{    
    if(input$pas == TRUE&&(input$Adv == TRUE))
    {
      data <- dataa()
      req(input$paste)
      data[, 3:ncol(data)] <- round(data[, 3:ncol(data)], 2)
      data<-(as.matrix(data))
      list<-c("Gene ID","Ensembl ID",input$show_vars2,input$show_vars5)
      #################################################################################
      geneid=strsplit(input$paste,",| |\n")
      geneid<-unlist(geneid)
      ######################################################################
      all_gtex_gens<-(data[,"Gene ID"])
      non_matched<-geneid[!(geneid %in% all_gtex_gens)]
      output$not_list <- renderUI({HTML((paste("The Gene Id",non_matched,"not found in the database","<br/>")))})
      ######################################################################
      table<-data[data[, 1] %in% geneid,list]
      table<-as.matrix(table)
      
      DT::datatable(table,class = 'cell-border stripe',
                    caption = HTML("Data Source: GTEx Analysis Release V8 <br/>
Displayed numbers represent median gene-level TPM by tissue"),filter="top"
      )
      
    }
    else{
      if(!is.null(input$file1))
     {
    req(input$file1)
    data <- dataa()
    data[, 3:ncol(data)] <- round(data[, 3:ncol(data)], 2)
    data<-(as.matrix(data))
    list<-c("Gene ID","Ensembl ID",input$show_vars2,input$show_vars5)
    #df <- read.csv(input$file1$datapath,header = F)
    geneid <- unique(rv$df[, 1])
    ######################################################################
    all_gtex_gens<-(data[,"Gene ID"])
    non_matched<-geneid[!(geneid %in% all_gtex_gens)]
    output$not_list <- renderUI({HTML((paste("The Gene Id",non_matched,"not found in the database","<br/>")))})
    ######################################################################
    table<-data[data[, 1] %in% geneid,list]
    table<-as.matrix(table)
    DT::datatable(table,class = 'cell-border stripe',
                  caption = HTML("Data Source: GTEx Analysis Release V8 <br/>
Displayed numbers represent median gene-level TPM by tissue"),filter="top"
    )
   
     }
          }}
  })
###############################End of First Page#####################
                                    
############################################################ Start of  Second Page###################################################
  
######******Correlations calculations******##########################################################################################
   output$mytable2 <- DT::renderDataTable({
     shiny::validate(
       need(input$show_vars32,"")
     )
     shiny::validate(
       need(input$show_vars33,"")
     )
     if (input$Adv == FALSE&&input$pas == FALSE)
       {
     dataa <- dataa()
     dataa[dataa <=1.5] <- 0
     drop(dataa)
     dataa$`Ensembl ID`<-NULL
     dataa$`Gene ID`<-NULL
     data<-as.matrix(dataa)
     list<-c(input$show_vars32,input$show_vars33)
     output$mydownload2=downloadHandler(filename = "Correlation matrix",content = function(file){write.csv(cor(data[,list]),file)})
    DT::datatable(cor(data[,list]),class = 'cell-border stripe',
                  caption = "Correlation matrix")
     }
     else{
       if(input$pas == TRUE&&(input$Adv == TRUE)){
         data <- dataa()
         data[data <=1.5] <- 0
         data<-(as.matrix(data))
         list<-c(input$show_vars32,input$show_vars33)
         geneid=strsplit(input$paste,",| |\n")
         geneid<-unlist(geneid)
         table<-data[data[, 1] %in% geneid,list]
     
         class(table) <- "numeric"
         table<-as.matrix(table)
         output$mydownload2=downloadHandler(filename = "Correlation matrix",content = function(file){write.csv(cor(table),file)})
         DT::datatable(cor(table),class = 'cell-border stripe',
                       caption = "Correlation matrix")
       }
       else{
         if(!is.null(input$file1))
     {
       data <- dataa()
       data[data <=1.5] <- 0
       data<-(as.matrix(data))
       list<-c(input$show_vars32,input$show_vars33)
       #df <- read.csv(input$file1$datapath,header = F)
       geneid <- unique(rv$df[, 1])
       table<-data[data[, 1] %in% geneid,list]
       class(table) <- "numeric"
       table<-as.matrix(table)
       output$mydownload2=downloadHandler(filename = "Correlation matrix",content = function(file){write.csv(cor(table),file)})
       DT::datatable(cor(table),class = 'cell-border stripe',
                     caption = "Correlation matrix")
     }}}
  })

  #########********Corelation Plots*****##########
   vals <- reactiveValues()
   
   output$plot1 <- renderPlot({
     shiny::validate(
       need(input$show_vars32,"Select an Accessible Tissue")
     )
     shiny::validate(
       need(input$show_vars33,"Select a Tissue of Interest")
     )
     if (input$Adv == FALSE&&input$pas == FALSE) { 
     dataa <- dataa()
     dataa[dataa <=1.5] <- 0
     drop(dataa)
     dataa$`Ensembl ID`<-NULL
     dataa$`Gene ID`<-NULL
     data<-as.matrix(dataa)
     list<-c(input$show_vars32,input$show_vars33)
     plot_scale<-cor(data[,list])
    
     if (length(list) <=10) {labsize <- 4}
     if (10 < length(list) & length(list)  <=20) {labsize <- 3}
     if (20 < length(list) & length(list) <=30) {labsize <- 2}
     if (30 < length(list) & length(list) <=60) {labsize <- 1}
    gg0<-ggcorrplot(as.matrix(cor(data[,list])),lab = TRUE,outline.col = "white"
                    ,title="Correlation Matrix Plot", lab_size = labsize)
      
     gg1<-gg0 +
       scale_fill_gradient2(low="blue", mid="white", high="red",midpoint = (min(plot_scale)+max(plot_scale))/2,limit=c(min(plot_scale),1)) +
       theme_minimal() +
       coord_equal() +
       labs(x="",y="",fill="Correlation coefficient") +
       theme(axis.text.x=element_text(size=labsize+10, angle=45, vjust=1, hjust=1, 
                                      margin=margin(0,0,0,0)),
             axis.text.y=element_text(size=labsize+10, margin=margin(0,0,0,0)),
             panel.grid.major=element_blank())
 
    vals$gg1 <- gg1
   
    
    print(gg1)
    }
     else{
       if(input$pas == TRUE&&(input$Adv == TRUE))
     {
       data <- dataa()
       data[data <=1.5] <- 0
       data<-(as.matrix(data))
       list<-c(input$show_vars32,input$show_vars33)
       geneid=strsplit(input$paste,",| |\n")
       geneid<-unlist(geneid)
       table<-data[data[, 1] %in% geneid,list]
       class(table) <- "numeric"
       table<-as.matrix(table)
       plot_scale<-cor(table)
       if (length(list) <=10) {labsize <- 4}
       if (10 < length(list) & length(list)  <=20) {labsize <- 3}
       if (20 < length(list) & length(list) <=30) {labsize <- 2}
       if (30 < length(list) & length(list) <=60) {labsize <- 1}
       gg0<-ggcorrplot(as.matrix(cor(table)),lab = TRUE,outline.col = "white"
                       ,title="Correlation Matrix Plot", lab_size = labsize)
       
       gg1<-gg0 +
         scale_fill_gradient2(low="blue", mid="white", high="red",midpoint = (min(plot_scale)+max(plot_scale))/2,limit=c(min(plot_scale),1)) +
         theme_minimal() +
         coord_equal() +
         labs(x="",y="",fill="Correlation coefficient") +
         theme(axis.text.x=element_text(size=labsize+10, angle=45, vjust=1, hjust=1, 
                                        margin=margin(0,0,0,0)),
               axis.text.y=element_text(size=labsize+10, margin=margin(0,0,0,0)),
               panel.grid.major=element_blank()) 
       vals$gg1 <- gg1
       print(gg1)
       
       }
       else{
         if(!is.null(input$file1))
         {
           data <- dataa()
           data[data <=1.5] <- 0
           data<-(as.matrix(data))
           list<-c(input$show_vars32,input$show_vars33)
           #df <- read.csv(input$file1$datapath,header = F)
           geneid <- unique(rv$df[, 1])
           table<-data[data[, 1] %in% geneid,list]
           class(table) <- "numeric"
           table<-as.matrix(table)
           plot_scale<-cor(table)
           if (length(list) <=10) {labsize <- 4}
           if (10 < length(list) & length(list)  <=20) {labsize <- 3}
           if (20 < length(list) & length(list) <=30) {labsize <- 2}
           if (30 < length(list) & length(list) <=60) {labsize <- 1}
           gg0<-ggcorrplot(as.matrix(cor(table)),lab = TRUE,outline.col = "white"
                           ,title="Correlation Matrix Plot", lab_size = labsize)
           
           gg1<-gg0 +
             scale_fill_gradient2(low="blue", mid="white", high="red",midpoint = (min(plot_scale)+max(plot_scale))/2,limit=c(min(plot_scale),1)) +
             theme_minimal() +
             coord_equal() +
             labs(x="",y="",fill="Correlation coefficient") +
             theme(axis.text.x=element_text(size=labsize+10, angle=45, vjust=1, hjust=1, 
                                            margin=margin(0,0,0,0)),
                   axis.text.y=element_text(size=labsize+10, margin=margin(0,0,0,0)),
                   panel.grid.major=element_blank()) 
           vals$gg1 <- gg1
           print(gg1)
           
         }}}
  })
   #################################save plot01
   output$down2 <- downloadHandler(
     filename =  function() {
       paste("Correlation-Matrix-Plot", input$var10,sep=".")
     },
     # content is a function with argument file. content writes the plot to the device
     content = function(file) {
       if(input$var10 == "png")
         {png((file),type="cairo", width=800, height=600)
         print(vals$gg1)
         dev.off()
         }# open the pdf device
       if(input$var10 == "pdf"){
         pdf(file, width=20, height=15) # open the pdf device
       print(vals$gg1)
       dev.off()  # turn the device off
         
     } }
   )
   ############################################# plot02 lower matrix ###
   vals2 <- reactiveValues()
  output$plot5 <- renderPlot({
    shiny::validate(
      need(input$show_vars32,"")
    )
    shiny::validate(
      need(input$show_vars33,"")
    )
    if (input$Adv == FALSE&&input$pas == FALSE)
      { 
    dataa <- dataa()
    dataa[dataa <=1.5] <- 0
    drop(dataa)
    dataa$`Ensembl ID`<-NULL
    dataa$`Gene ID`<-NULL
    data<-as.matrix(dataa)
    list<-c(input$show_vars32,input$show_vars33)
    plot_scale<-cor(data[,list])
    if (length(list) <=10) {labsize <- 4}
    if (10 < length(list) & length(list)  <=20) {labsize <- 3}
    if (20 < length(list) & length(list) <=30) {labsize <- 2}
    if (30 < length(list) & length(list) <=60) {labsize <- 1}
    gg0<-ggcorrplot(as.matrix(cor(data[,list])),lab = TRUE,outline.col = "white",hc.order = TRUE, 
                    title="The correlation matrix: Lower triangle",type = "lower",lab_size = labsize)
    gg2<-gg0+scale_fill_gradient2(low="blue", mid="violet", high="red",midpoint = (min(plot_scale)+max(plot_scale))/2,limit=c(min(plot_scale),1)) +
      theme_minimal() +
      coord_equal() +
      labs(x="",y="",fill="Corr") +
      theme(axis.text.x=element_text(size=13, angle=45, vjust=1, hjust=1, 
                                     margin=margin(-3,0,0,0)),
            axis.text.y=element_text(size=13, margin=margin(0,-3,0,0)),
            panel.grid.major=element_blank()) 
    vals2$gg2 <- gg2
    print(gg2)
    }
    else{
      if(input$pas == TRUE&&(input$Adv == TRUE))
    {
      data <- dataa()
      data[data <=1.5] <- 0
      data<-(as.matrix(data))
      list<-c(input$show_vars32,input$show_vars33)
      geneid=strsplit(input$paste,",| |\n")
      geneid<-unlist(geneid)
      table<-data[data[, 1] %in% geneid,list]
      class(table) <- "numeric"
      table<-as.matrix(table)
      plot_scale<-cor(table)
      if (length(list) <=10) {labsize <- 4}
      if (10 < length(list) & length(list)  <=20) {labsize <- 3}
      if (20 < length(list) & length(list) <=30) {labsize <- 2}
      if (30 < length(list) & length(list) <=60) {labsize <- 1}
      gg0<-ggcorrplot(as.matrix(cor(table)),lab = TRUE,outline.col = "white",hc.order = TRUE, 
                      title="The correlation matrix: Lower triangle",type = "lower",lab_size = labsize)
      gg2<-gg0+scale_fill_gradient2(low="blue", mid="violet", high="red",midpoint = (min(plot_scale)+max(plot_scale))/2,limit=c(min(plot_scale),1)) +
        theme_minimal() +
        coord_equal() +
        labs(x="",y="",fill="Corr") +
        theme(axis.text.x=element_text(size=13, angle=45, vjust=1, hjust=1, 
                                       margin=margin(-3,0,0,0)),
              axis.text.y=element_text(size=13, margin=margin(0,-3,0,0)),
              panel.grid.major=element_blank()) 
      vals2$gg2 <- gg2
      print(gg2)
    }
    else{
      if(!is.null(input$file1))
      {
        data <- dataa()
        data[data <=1.5] <- 0
        data<-(as.matrix(data))
        list<-c(input$show_vars32,input$show_vars33)
        #df <- read.csv(input$file1$datapath,header = F)
        geneid <- unique(rv$df[, 1])
        table<-data[data[, 1] %in% geneid,list]
        class(table) <- "numeric"
        table<-as.matrix(table)
        plot_scale<-cor(table)
        if (length(list) <=10) {labsize <- 4}
        if (10 < length(list) & length(list)  <=20) {labsize <- 3}
        if (20 < length(list) & length(list) <=30) {labsize <- 2}
        if (30 < length(list) & length(list) <=60) {labsize <- 1}
        gg0<-ggcorrplot(as.matrix(cor(table)),lab = TRUE,outline.col = "white",hc.order = TRUE, 
                        title="The correlation matrix: Lower triangle",type = "lower",lab_size = labsize)
        gg2<-gg0+scale_fill_gradient2(low="blue", mid="violet", high="red",midpoint = (min(plot_scale)+max(plot_scale))/2,limit=c(min(plot_scale),1)) +
          theme_minimal() +
          coord_equal() +
          labs(x="",y="",fill="Corr") +
          theme(axis.text.x=element_text(size=13, angle=45, vjust=1, hjust=1, 
                                         margin=margin(-3,0,0,0)),
                axis.text.y=element_text(size=13, margin=margin(0,-3,0,0)),
                panel.grid.major=element_blank()) 
        vals2$gg2 <- gg2
        print(gg2)
      }}}
    
  })
###############################################Save plot2
  output$down3 <- downloadHandler(
    filename =  function() {
      paste("The-correlation-matrix-Lower-triangle",input$var10,sep=".")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      if(input$var10 == "pdf"){
        pdf(file,width=20, height=15) # open the pdf device
        print(vals2$gg2)
        dev.off()  # turn the device off
      }
      if(input$var10 == "png")
      {png((file),type="cairo",width=800, height=600)
        print(vals2$gg2)
        dev.off()
      }# open the pdf device
    
    } 
  )
  ####################################Gene Expression Page Venn Diagram page 3
  
    output$myImagevenn <- renderImage({
    shiny::validate(need(input$show_vars23,"Enter one Accessible Tissue"))
    shiny::validate(need(input$show_vars22,"Enter one Tissue of Interest"))
    ######################################
    
    if (input$Adv == FALSE&&input$pas == FALSE)
    {
      data <- dataa()
      data<-na_if(data, 0)
      data[data <=1.5] <- NA
      data<-(as.matrix(data))
      list1<-c("Gene ID",input$show_vars23)
      list2<-c("Gene ID",input$show_vars22)
      act<-data[,list1]
      act<-as.data.frame(act)
      act<-act[rowSums(is.na(act))<(length(act)-1),]
  
      titable<-(data[,list2])
      titable<-as.data.frame(titable)
      titable<-titable[rowSums(is.na(titable))<(length(titable)-1),]
     
   #********************    
      AT<-as.array(act[,"Gene ID"])
      TI<-as.array(titable[,"Gene ID"])
      PI<-as.array(data[,"Gene ID"])
      write.table(AT,'AT')
      write.table(TI,'TI')
      write.table(PI,'PI')
      #****************************************************************
      venn.plot <- reactive({
        overrideTriple=T
        venn.diagram(
        x = list(
          "PI"=PI,
          "AT" = AT,
          "TI" = TI
          
        ),
        
       main = "",
      filename =outfile, output=TRUE,lty =3,
        na = "remove",imagetype="png",
        col = "transparent",
        fill = c("cornflowerblue", "darkgreen", "red"),
        alpha = c(0.5,0.5, 0.5),
        cex = 0,
        fontfamily = "serif",
        fontface = "plain",
        cat.col = c("black", "black", "black"),
        cat.cex = 0,
        cat.pos = 0,
        cat.dist = 0.07,
        cat.fontfamily = "serif",
        rotation.degree = 270,cat.default.pos = "text",
      euler.d=TRUE,
        scaled = TRUE,
        margin = 0.2
       )
      })
      outfile <- tempfile(fileext = '.png')
      png(outfile, width = 500, height = 500,type="cairo")
      venn.plot()
      dev.off()
      
      list(src = outfile,
           contentType = 'image/png',
           width = 450,
           height = 450,
           alt = "This is alternate text")}
    else{    
      if(input$pas == TRUE&&(input$Adv == TRUE))
      {
        
        data <- dataa()
        data<-na_if(data, 0)
        data[data <=1.5] <- NA
        req(input$paste)
        data<-(as.matrix(data))
        list1<-c("Gene ID",input$show_vars23)
        list2<-c("Gene ID",input$show_vars22)

        geneid=strsplit(input$paste,",| |\n")
        geneid<-unlist(geneid)
        act<-data[data[, 1] %in% geneid,list1]
        act<-as.data.frame(act)
        act<-act[rowSums(is.na(act))<(length(act)-1),]
        titable<-data[data[, 1] %in% geneid,list2]
        titable<-as.data.frame(titable)
        titable<-titable[rowSums(is.na(titable))<(length(titable)-1),]
        #*************************************    
        AT<-as.array(act[,"Gene ID"])
        TI<-as.array(titable[,"Gene ID"])
        PI<-as.array(data[,"Gene ID"])
        #****************************************************************
        venn.plot <- reactive({venn.diagram(
          x = list(
            "PI"=PI, "AT" = AT,
            "TI" = TI
            
          ),
          main = "",
          filename =outfile, output=TRUE,lty =3,
          na = "remove",imagetype="png",
          col = "transparent",
          fill = c("cornflowerblue", "pink", "yellow"),
          alpha = c(0.5,0.5, 0.5),
          cex = 1.5,
          fontfamily = "serif",
          fontface = "plain",
          cat.col = c("black", "black", "black"),
          cat.cex = 1.5,
          cat.pos = 0,
          cat.dist = 0.07,
          cat.fontfamily = "serif",
          rotation.degree = 270,cat.default.pos = "text",
          scaled = FALSE,
          margin = 0.2
        )
        })
        outfile <- tempfile(fileext = '.png')
        png(outfile, width = 500, height = 500,type="cairo")
        venn.plot()
        dev.off()
        
        list(src = outfile,
             contentType = 'image/png',
             width = 450,
             height = 450,
             alt = "This is alternate text")
        
      }
      else{
        if(!is.null(input$file1))
        {
          shiny::validate(need(input$file1,""))
          req(input$file1)
          data <- dataa()
          data<-na_if(data, 0)
          data[data <=1.5] <- NA
          data<-(as.matrix(data))
          list1<-c("Gene ID",input$show_vars23)
          list2<-c("Gene ID",input$show_vars22)
          ########################################################
          #####################################################
          geneid <- unique(rv$df[, 1])
          act<-data[data[, 1] %in% geneid,list1]
          act<-as.data.frame(act)
          act<-act[rowSums(is.na(act))<(length(act)-1),]
          titable<-data[data[, 1] %in% geneid,list2]
          AT<-as.array(act[,"Gene ID"])
          TI<-as.array(titable[,"Gene ID"])
          PI<-as.array(data[,"Gene ID"])
          #****************************************************************
          venn.plot <- reactive({venn.diagram(
            x = list(
              "PI"=PI, "AT" = AT,
              "TI" = TI
              
            ),
            main = "",
            filename =outfile, output=TRUE,lty =3,
            na = "remove",imagetype="png",
            col = "transparent",
            fill = c("cornflowerblue", "pink", "yellow"),
            alpha = c(0.5,0.5, 0.5),
            cex = 1.5,
            fontfamily = "serif",
            fontface = "plain",
            cat.col = c("black", "black", "black"),
            cat.cex = 1.5,
            cat.pos = 0,
            cat.dist = 0.07,
            cat.fontfamily = "serif",
            rotation.degree = 270,cat.default.pos = "text",
            scaled = FALSE,
            margin = 0.2
          )
          })
          outfile <- tempfile(fileext = '.png')
          png(outfile, width = 500, height = 500,type="cairo")
          venn.plot()
          dev.off()
          
          list(src = outfile,
               contentType = 'image/png',
               width = 450,
               height = 450,
               alt = "This is alternate text")
          
          
          
        }}}}, deleteFile = TRUE)
############################################# Gene TABLE ##########################################################
    output$mytable20 <- DT::renderDataTable({
      shiny::validate(need(input$show_vars23,"Enter one Accessible Tissue"))
      shiny::validate(need(input$show_vars22,"Enter one Tissue of Interest"))
      if (input$Adv == FALSE&&input$pas == FALSE) {
        data <- dataa()
        #data<-na_if(data, 0) #remove column if it zero
        #data[data <=1.5] <- NA
        data[, 3:ncol(data)] <- round(data[, 3:ncol(data)], 2)
        data<-(as.matrix(data))
        data<-trimws(data, "l") #remove space 
        list1<-c("Ensembl ID","Gene ID",input$show_vars23)
        list2<-c("Ensembl ID","Gene ID",input$show_vars22)
        act<-data[,list1]
        act<-as.data.frame(act)
        #act<-act[rowSums(is.na(act))<(length(act)-2),]
        act<-as.matrix(act)
        titable<-(data[,list2])
        #titable<-titable[rowSums(is.na(titable))<(length(titable)-2),]
        titable<-as.matrix(titable)
        PI<-as.array(data[,c("Ensembl ID","Gene ID")])
        #*************************************
        AT<-as.data.frame(act)
        AT$Type <- ifelse(rowSums(AT[-1:-2] >= 1.5), 'AT', '')
        TI<-as.data.frame(titable)
        TI$Type <- ifelse(rowSums(TI[-1:-2] >= 1.5), 'TI', '')
        PI<-cbind(PI,Type='PI')
        tableout<-Reduce(function(x, y) merge(x, y, by = c("Ensembl ID","Gene ID")), list(AT,TI, PI))%>%unite("Type", starts_with("Type"), sep = " ")
        output$mydownload20=downloadHandler(filename = "Selected_Results.csv",content = function(file){write.csv(tableout,file)})
        DT::datatable((as.matrix(tableout)),class = 'cell-border stripe',
                      caption = "List of Genes with Median gene-level TPM by tissue",options = list(
                        paging = TRUE,
                        pageLength = 5,
                        columnDefs = list(list(className = "dt-center", targets = "_all"))
                      ))}
      else{
        if(input$pas == TRUE&&(input$Adv == TRUE))
        {
          data <- dataa()
          data<-na_if(data, 0)
          data[data <=1.5] <- NA
          data<-(as.matrix(data))
          list1<-c("Ensembl ID","Gene ID",input$show_vars23)
          list2<-c("Ensembl ID","Gene ID",input$show_vars22)
          act<-data[,list1]
          act<-as.data.frame(act)
          act<-act[rowSums(is.na(act))<(length(act)-2),]
          act<-as.matrix(act)
          titable<-(data[,list2])
          titable<-as.data.frame(titable)
          titable<-titable[rowSums(is.na(titable))<(length(titable)-2),]
          titable<-as.matrix(titable)
          #********************    
          AT<-as.array(act[,c("Ensembl ID","Gene ID")])
          TI<-as.array(titable[,c("Ensembl ID","Gene ID")])
          PI<-as.array(data[,c("Ensembl ID","Gene ID")])
          
          #****************************************************************
          TI<-cbind(TI,Type='TI')
          AT<-cbind(AT,Type='AT')
          PI<-cbind(PI,Type='PI')
          colnames(AT) <- c("Gene_ID","Gene_Name","Type")
          colnames(TI) <- c("Gene_ID","Gene_Name","Type")
          colnames(PI) <- c("Gene_ID","Gene_Name","Type")
          TI<-data.frame(TI)
          AT<-data.frame(AT)
          PI<-data.frame(PI)
          aa<-list(AT,TI, PI) %>% purrr::reduce(dplyr::full_join, by = c("Gene_ID","Gene_Name")) %>% tidyr::unite(type, dplyr::starts_with("Type"), sep = "/", na.rm = TRUE)
          
          ########################
          geneid=strsplit(input$paste,",| |\n")
          geneid<-unlist(geneid)
          
          gene_id<-merge(geneid,aa,by.x = 1, by.y = "Gene_Name")
          ################################
          
          output$mydownload20=downloadHandler(filename = "Selected_Results.csv",content = function(file){write.csv(gene_id,file)})
          DT::datatable((as.matrix(gene_id)),class = 'cell-border stripe',
                        caption = "List of Genes with Median gene-level TPM by tissue",colnames =c("Gene Name","Gene ID","Type"),options = list(
                          paging = TRUE,
                          pageLength = 5,
                          columnDefs = list(list(className = "dt-center", targets = "_all"))
                        ))
        }
        else{
          if(!is.null(input$file1))
          {
            data <- dataa()
            data<-na_if(data, 0)
            data[data <=1.5] <- NA
            data<-(as.matrix(data))
            list1<-c("Ensembl ID","Gene ID",input$show_vars23)
            list2<-c("Ensembl ID","Gene ID",input$show_vars22)
            act<-data[,list1]
            act<-as.data.frame(act)
            act<-act[rowSums(is.na(act))<(length(act)-2),]
            act<-as.matrix(act)
            titable<-(data[,list2])
            titable<-as.data.frame(titable)
            titable<-titable[rowSums(is.na(titable))<(length(titable)-2),]
            titable<-as.matrix(titable)
            #********************    
            AT<-as.array(act[,c("Ensembl ID","Gene ID")])
            TI<-as.array(titable[,c("Ensembl ID","Gene ID")])
            PI<-as.array(data[,c("Ensembl ID","Gene ID")])
            
            #****************************************************************
            TI<-cbind(TI,Type='TI')
            AT<-cbind(AT,Type='AT')
            PI<-cbind(PI,Type='PI')
            colnames(AT) <- c("Gene_ID","Gene_Name","Type")
            colnames(TI) <- c("Gene_ID","Gene_Name","Type")
            colnames(PI) <- c("Gene_ID","Gene_Name","Type")
            TI<-data.frame(TI)
            AT<-data.frame(AT)
            PI<-data.frame(PI)
            aa<-list(AT,TI, PI) %>% purrr::reduce(dplyr::full_join, by = c("Gene_ID","Gene_Name")) %>% tidyr::unite(type, dplyr::starts_with("Type"), sep = "/", na.rm = TRUE)
           
            ########################
           
            geneid <- unique(rv$df[, 1])
          
            gene_id<-merge(geneid,aa,by.x = 1, by.y = "Gene_Name")
            ################################
            
            output$mydownload20=downloadHandler(filename = "Selected_Results.csv",content = function(file){write.csv(gene_id,file)})
            DT::datatable((as.matrix(gene_id)),class = 'cell-border stripe',
                          caption = "List of Genes with Median gene-level TPM by tissue",colnames =c("Gene Name","Gene ID","Type"),options = list(
                            paging = TRUE,
                            pageLength = 5,
                            columnDefs = list(list(className = "dt-center", targets = "_all"))
                          ))
          }}}
    })
    
    
#############################################transcript page
################################################
  Tdata<-reactive({
    if (input$Gene_list == 'ca')
    { fread('transcript/Cardiac-Arrythmia',sep = "\t",header = T,check.names = FALSE,na.strings ="", stringsAsFactors= F)
    }
    else{
    if((input$Adv == TRUE)) 
    { fread('transcript/All',sep = "\t",header = T,check.names = FALSE,na.strings ="", stringsAsFactors= F)
    }
      
    else{
      if (input$Gene_list == 'ndd')
      {
        fread('transcript/sysID',sep = "\t",header = T,check.names = FALSE,na.strings ="", stringsAsFactors= F)
      }
      else{
        if (input$Gene_list == 'ob')
        {
          fread('transcript/Obesity',sep = "\t",header = T,check.names = FALSE,na.strings ="", stringsAsFactors= F)
        }
      else{
        if (input$Gene_list == 'blo-abn')
        {
          fread('transcript/Blood_abnormality',sep = "\t",header = T,check.names = FALSE,na.strings ="", stringsAsFactors= F)
        }
        else{
          if (input$Gene_list == 'bre-abn')
          {
            fread('transcript/Breast_abnormality',sep = "\t",header = T,check.names = FALSE, na.strings ="", stringsAsFactors= F)
          }
          else{
            if (input$Gene_list == 'abd-abn')
            {
              fread('transcript/Abdomen-abnormality',sep = "\t",header = T,check.names = FALSE,na.strings ="", stringsAsFactors= F)
            }
            else{
              if (input$Gene_list == 'car-abn')
              {
                fread('transcript/Cardiovascular-abnormality',sep = "\t",header = T,check.names = FALSE,na.strings ="", stringsAsFactors= F)
              }
              else{
                if (input$Gene_list == 'con-t-ab')
                {
                  fread('transcript/Connective_tissue_abnormality',sep = "\t",header = T,check.names = FALSE,na.strings ="", stringsAsFactors= F)
                }
                else{
                  if (input$Gene_list == 'ear-ab')
                  {
                    fread('transcript/Ear-abbnormality',sep = "\t",header = T,check.names = FALSE,na.strings ="", stringsAsFactors= F)
                  }
                  else{
                    if (input$Gene_list == 'end-abn')
                    {
                      fread('transcript/Endocrine-abnormality',sep = "\t",header = T,check.names = FALSE,na.strings ="", stringsAsFactors= F)
                    }
                    else{
                      if (input$Gene_list == 'eye-abn')
                      {
                        fread('transcript/Eye-abnormality',sep = "\t",header = T,check.names = FALSE,na.strings ="", stringsAsFactors= F)
                      }
                      else{
                        if (input$Gene_list == 'gen-abn')
                        {
                          fread('transcript/Genitourinary_abnormality',sep = "\t",header = T,check.names = FALSE,na.strings ="", stringsAsFactors= F)
                        }
                        else{
                          if (input$Gene_list == 'gro-abn')
                          {
                            fread('transcript/Growth-abnormality',sep = "\t",header = T,check.names = FALSE,na.strings ="", stringsAsFactors= F)
                          }
                          else{
                            if (input$Gene_list == 'hea-n-abn')
                            {
                              fread('transcript/Head-neck-abnormality',sep = "\t",header = T,check.names = FALSE,na.strings ="", stringsAsFactors= F)
                            }
                            else{
                              if (input$Gene_list == 'imm-abn')
                              {
                                fread('transcript/Immune-abnormality',sep = "\t",header = T,check.names = FALSE,na.strings ="", stringsAsFactors= F)
                              }
                              else{
                                if (input$Gene_list == 'int-abn')
                                {
                                  fread('transcript/Integument-abbnormality',sep = "\t",header = T,check.names = FALSE,na.strings ="", stringsAsFactors= F)
                                }
                                else{
                                  if (input$Gene_list == 'lim-abn')
                                  {
                                    fread('transcript/Limbs_abnormality',sep = "\t",header = T,check.names = FALSE,na.strings ="", stringsAsFactors= F)
                                  }
                                  else{
                                    if (input$Gene_list == 'met-abn')
                                    {
                                      fread('transcript/Metabbolism_abnormality',sep = "\t",header = T,check.names = FALSE,na.strings ="", stringsAsFactors= F)
                                    }
                                    else{
                                      if (input$Gene_list == 'mus-abn')
                                      {
                                        fread('transcript/Musculature-abnormality',sep = "\t",header = T,check.names = FALSE,na.strings ="", stringsAsFactors= F)
                                      }
                                      else{
                                        if (input$Gene_list == 'neo')
                                        {
                                          fread('transcript/Neoplasm',sep = "\t",header = T,check.names = FALSE,na.strings ="", stringsAsFactors= F)
                                        }
                                        else{
                                          if (input$Gene_list == 'ner-s-abn')
                                          {
                                            fread('transcript/Nervous-system-abnormality',sep = "\t",header = T,check.names = FALSE,na.strings ="", stringsAsFactors= F)
                                          }
                                          else{
                                            if (input$Gene_list == 'pre-abn')
                                            {
                                              fread('transcript/Prenatal-abnormality',sep = "\t",header = T,check.names = FALSE,na.strings ="", stringsAsFactors= F)
                                            }
                                            else{
                                              if (input$Gene_list == 'res-abn')
                                              {
                                                fread('transcript/Respiratory_abnormality',sep = "\t",header = T,check.names = FALSE,na.strings ="", stringsAsFactors= F)
                                              }
                                              else{
                                                if (input$Gene_list == 'ske-abn')
                                                {
                                                  fread('transcript/Skeletal-abnormality',sep = "\t",header = T,check.names = FALSE,na.strings ="", stringsAsFactors= F)
                                                }
                                                else{
                                                  if (input$Gene_list == 'tho-abn')
                                                  {
                                                    fread('transcript/Thoracic-abnormality',sep = "\t",header = T,check.names = FALSE,na.strings ="", stringsAsFactors= F)
                                                  }
                                                  else{
                                                    if (input$Gene_list == 'voi-abn')
                                                    {
                                                      fread('transcript/Voice-abnormality',sep = "\t",header = T,check.names = FALSE,na.strings ="", stringsAsFactors= F)
                                                    }
                                                    else
                                                    {
                                                      if (input$Gene_list == 'all')
                                                      {
                                                        fread('transcript/All',sep = "\t",header = T,check.names = FALSE,na.strings ="", stringsAsFactors= F)
                                                      }
                                                    }
                                                  }}}}}}}}}}}}}}}}}}}}}}}}}}
    
  })
########################################################################### transcript median
    Tmedian<-reactive({
      if(input$Gene_list == 'ca') 
      { fread('Transcript-Median/Cardiac-Arrythmia.txt',sep = "\t",header = T,check.names = FALSE,na.strings ="", stringsAsFactors= F)
      }
      else{
      if((input$Adv == TRUE)) 
      { fread('Transcript-Median/all.txt',sep = "\t",header = T,check.names = FALSE,na.strings ="", stringsAsFactors= F)
      }
      else{
        if (input$Gene_list == 'ndd')
        {
          fread('Transcript-Median/sysID.txt',sep = "\t",header = T,check.names = FALSE,na.strings ="", stringsAsFactors= F)
        }
        else{
          if (input$Gene_list == 'ob')
          {
            fread('Transcript-Median/Obesity.txt',sep = "\t",header = T,check.names = FALSE,na.strings ="", stringsAsFactors= F)
          }
        else{
          if (input$Gene_list == 'blo-abn')
          {
            fread('Transcript-Median/blood-abnormality.txt',sep = "\t",header = T,check.names = FALSE,na.strings ="", stringsAsFactors= F)
          }
          else{
            if (input$Gene_list == 'bre-abn')
            {
              fread('Transcript-Median/Breast_abnormality.txt',sep = "\t",header = T,check.names = FALSE, na.strings ="", stringsAsFactors= F)
            }
            else{
              if (input$Gene_list == 'abd-abn')
              {
                fread('Transcript-Median/abdoman-abnormality.txt',sep = "\t",header = T,check.names = FALSE,na.strings ="", stringsAsFactors= F)
              }
              else{
                if (input$Gene_list == 'car-abn')
                {
                  fread('Transcript-Median/Cardiovascular-abnormality.txt',sep = "\t",header = T,check.names = FALSE,na.strings ="", stringsAsFactors= F)
                }
                else{
                  if (input$Gene_list == 'con-t-ab')
                  {
                    fread('Transcript-Median/Connective_tissue_abnormality.txt',sep = "\t",header = T,check.names = FALSE,na.strings ="", stringsAsFactors= F)
                  }
                  else{
                    if (input$Gene_list == 'ear-ab')
                    {
                      fread('Transcript-Median/Ear-abbnormality.txt',sep = "\t",header = T,check.names = FALSE,na.strings ="", stringsAsFactors= F)
                    }
                    else{
                      if (input$Gene_list == 'end-abn')
                      {
                        fread('Transcript-Median/Endocrine-abnormality.txt',sep = "\t",header = T,check.names = FALSE,na.strings ="", stringsAsFactors= F)
                      }
                      else{
                        if (input$Gene_list == 'eye-abn')
                        {
                          fread('Transcript-Median/Eye-abnormality.txt',sep = "\t",header = T,check.names = FALSE,na.strings ="", stringsAsFactors= F)
                        }
                        else{
                          if (input$Gene_list == 'gen-abn')
                          {
                            fread('Transcript-Median/Genitourinary_abnormality.txt',sep = "\t",header = T,check.names = FALSE,na.strings ="", stringsAsFactors= F)
                          }
                          else{
                            if (input$Gene_list == 'gro-abn')
                            {
                              fread('Transcript-Median/Growth-abnormality.txt',sep = "\t",header = T,check.names = FALSE,na.strings ="", stringsAsFactors= F)
                            }
                            else{
                              if (input$Gene_list == 'hea-n-abn')
                              {
                                fread('Transcript-Median/Head-neck-abnormality.txt',sep = "\t",header = T,check.names = FALSE,na.strings ="", stringsAsFactors= F)
                              }
                              else{
                                if (input$Gene_list == 'imm-abn')
                                {
                                  fread('Transcript-Median/Immune-abnormality.txt',sep = "\t",header = T,check.names = FALSE,na.strings ="", stringsAsFactors= F)
                                }
                                else{
                                  if (input$Gene_list == 'int-abn')
                                  {
                                    fread('Transcript-Median/Integument-abbnormality.txt',sep = "\t",header = T,check.names = FALSE,na.strings ="", stringsAsFactors= F)
                                  }
                                  else{
                                    if (input$Gene_list == 'lim-abn')
                                    {
                                      fread('Transcript-Median/Limbs_abnormality.txt',sep = "\t",header = T,check.names = FALSE,na.strings ="", stringsAsFactors= F)
                                    }
                                    else{
                                      if (input$Gene_list == 'met-abn')
                                      {
                                        fread('Transcript-Median/Metabbolism_abnormality.txt',sep = "\t",header = T,check.names = FALSE,na.strings ="", stringsAsFactors= F)
                                      }
                                      else{
                                        if (input$Gene_list == 'mus-abn')
                                        {
                                          fread('Transcript-Median/Musculature-abnormality.txt',sep = "\t",header = T,check.names = FALSE,na.strings ="", stringsAsFactors= F)
                                        }
                                        else{
                                          if (input$Gene_list == 'neo')
                                          {
                                            fread('Transcript-Median/Neoplasm.txt',sep = "\t",header = T,check.names = FALSE,na.strings ="", stringsAsFactors= F)
                                          }
                                          else{
                                            if (input$Gene_list == 'ner-s-abn')
                                            {
                                              fread('Transcript-Median/Nervous-system-abnormality.txt',sep = ",",header = T,check.names = FALSE,na.strings ="", stringsAsFactors= F)
                                            }
                                            else{
                                              if (input$Gene_list == 'pre-abn')
                                              {
                                                fread('Transcript-Median/Prenatal-abnormality.txt',sep = "\t",header = T,check.names = FALSE,na.strings ="", stringsAsFactors= F)
                                              }
                                              else{
                                                if (input$Gene_list == 'res-abn')
                                                {
                                                  fread('Transcript-Median/Respiratory_abnormality.txt',sep = "\t",header = T,check.names = FALSE,na.strings ="", stringsAsFactors= F)
                                                }
                                                else{
                                                  if (input$Gene_list == 'ske-abn')
                                                  {
                                                    fread('Transcript-Median/Skeletal-abnormality.txt',sep = "\t",header = T,check.names = FALSE,na.strings ="", stringsAsFactors= F)
                                                  }
                                                  else{
                                                    if (input$Gene_list == 'tho-abn')
                                                    {
                                                      fread('Transcript-Median/Thoracic-abnormality.txt',sep = "\t",header = T,check.names = FALSE,na.strings ="", stringsAsFactors= F)
                                                    }
                                                    else{
                                                      if (input$Gene_list == 'voi-abn')
                                                      {
                                                        fread('Transcript-Median/Voice-abnormality.txt',sep = "\t",header = T,check.names = FALSE,na.strings ="", stringsAsFactors= F)
                                                      }
                                                      else
                                                      {
                                                        if (input$Gene_list == 'all')
                                                        {
                                                          fread('Transcript-Median/all.txt',sep = "\t",header = T,check.names = FALSE,na.strings ="", stringsAsFactors= F)
                                                        }
                                                      }
                                                      
                                                    }}}}}}}}}}}}}}}}}}}}}}}}}}
      
    })
    
######################################### Transcript Venn Diagram
output$Vennout <- renderImage({
  if (input$Adv == FALSE&&input$pas == FALSE) {
    
    #########################################################
    TMdata <- Tmedian()
    TMdata<-(as.matrix(TMdata))
    TMdata<-trimws(TMdata, "l")
    list<-c("Gene ID","Ensembl Gene ID","Ensembl Transcript ID",input$show_vars12)
    list2<-c("Gene ID","Ensembl Gene ID","Ensembl Transcript ID",input$show_vars15)
    ACE<-TMdata[,list]
    TIE<-TMdata[,list2]
    ACE<-as.data.frame(ACE)
    TIE<-as.data.frame(TIE)
    AT<-ACE[Reduce(`|`, lapply(ACE[-(1:3)], '>=', 1.5)),]
    TI<-TIE[Reduce(`|`, lapply(TIE[-(1:3)], '>=', 1.5)),]
    AT<-as.array(AT[,"Ensembl Transcript ID"])
    TI<-as.array(TI[,"Ensembl Transcript ID"])
    #########################################################
      venn.plot <- reactive({venn.diagram(
        x = list(
          "AT" = AT,
          "TI" = TI
        ),
        main = "",
        filename =outfile, output=TRUE,
        lty = 3,na = "remove",imagetype="png",
        col="transparent",
        fill = c("cornflowerblue", "pink"),
        alpha = c(0.5,0.5),
        #label.col = "black",
        cex=0,
        fontface = "plain",
        cat.col = c("black", "black"),
        cat.cex = 0,
        cat.fontfamily = "serif",
        cat.fontface = "plain",
        cat.dist = c(0.05, 0.05),
        cat.pos = c(-20, 14),
        cat.default.pos = "text",
        scaled = TRUE
      )
      })
      outfile <- tempfile(fileext = '.png')
      png(outfile, width = 500, height = 500,type="cairo")
      venn.plot()
       dev.off()
      
      list(src = outfile,
           contentType = 'image/png',
           width = 500,
           height = 500,
           alt = "This is alternate text")
  }
  else{
    if(input$pas == TRUE&&(input$Adv == TRUE))
  {
    Tdata <- Tdata()
    checkdata<-fread("transcript/tpm_geneID",header = T,sep = "\t")
    Tdata<-as.matrix(Tdata)
    list1<-rbind(input$show_vars12)
    list2<-(input$show_vars15)
    AT<-as.array(Tdata[,list1])
    TI<-as.array(Tdata[,list2])
    bb<-paste((t(AT)))
    
    TI<-merge(TI,checkdata,by.x = 1, by.y = 1)
    AT<-merge(bb,checkdata,by.x = 1, by.y = 1)
    
    TI<-cbind(TI,Type='TI')
    AT<-cbind(AT,Type='AT')
    
    colnames(AT) <- c("Trascript_ID", "Gene_ID","Gene_Name","Type")
    colnames(TI) <- c("Trascript_ID", "Gene_ID","Gene_Name","Type")
    TI<-data.frame(TI)
    AT<-data.frame(AT)
    aa<-(dplyr::full_join(TI,AT, by = c('Gene_Name','Trascript_ID','Gene_ID')) %>% tidyr::unite(Type, Type.x, Type.y, sep = '/', na.rm = TRUE))
    ########################
    geneid=strsplit(input$paste,",| |\n")
    geneid<-unlist(geneid)
    checkdata<-fread("transcript/tpm_geneID",header = T,sep = "\t")
    gene_id<-merge(geneid,aa,by.x = 1, by.y = "Gene_Name")
    gene_id<-as.matrix(gene_id)
    AT<-cbind(gene_id[grep("AT", gene_id[,"Type"]), "Trascript_ID"])
    TI<-cbind(gene_id[grep("TI", gene_id[,"Type"]), "Trascript_ID"])
    ################################
    #AT<-as.array(Tdata[,list1])
    #TI<-as.array(Tdata[,list2])
    venn.plot <- reactive({venn.diagram(
      x = list(
        "AT" = AT,
        "TI" = TI
      ),
      main = "",
      filename =outfile, output=TRUE,
      lty = 3,na = "remove",imagetype="png",
      col = "transparent",
      fill = c("cornflowerblue", "pink"),
      alpha = c(0.5,0.5),
      label.col = "black",
      cex=1.5,
      fontface = "plain",
      cat.col = c("black", "black"),
      cat.cex = 1.5,
      cat.fontfamily = "serif",
      cat.fontface = "plain",
      cat.dist = c(0.05, 0.05),
      cat.pos = c(-20, 14),
      cat.default.pos = "text",
      scaled = TRUE
    )
    })
    outfile <- tempfile(fileext = '.png')
    png(outfile, width = 500, height = 500,type="cairo")
    venn.plot()
    dev.off()
    
    list(src = outfile,
         contentType = 'image/png',
         width = 500,
         height = 500,
         alt = "This is alternate text")
    }
    else{
      if(!is.null(input$file1))
      {
        Tdata <- Tdata()
        checkdata<-fread("transcript/tpm_geneID",header = T,sep = "\t")
        Tdata<-as.matrix(Tdata)
        list1<-rbind(input$show_vars12)
        list2<-(input$show_vars15)
        AT<-as.array(Tdata[,list1])
        TI<-as.array(Tdata[,list2])
        bb<-paste((t(AT)))
        
        TI<-merge(TI,checkdata,by.x = 1, by.y = 1)
        AT<-merge(bb,checkdata,by.x = 1, by.y = 1)
        
        TI<-cbind(TI,Type='TI')
        AT<-cbind(AT,Type='AT')
        
        colnames(AT) <- c("Trascript_ID", "Gene_ID","Gene_Name","Type")
        colnames(TI) <- c("Trascript_ID", "Gene_ID","Gene_Name","Type")
        TI<-data.frame(TI)
        AT<-data.frame(AT)
        aa<-(dplyr::full_join(TI,AT, by = c('Gene_Name','Trascript_ID','Gene_ID')) %>% tidyr::unite(Type, Type.x, Type.y, sep = '/', na.rm = TRUE))
        ########################
        #df <- read.csv(input$file1$datapath,header = F)
        geneid <- unique(rv$df[, 1])
        checkdata<-fread("transcript/tpm_geneID",header = T,sep = "\t")
        gene_id<-merge(geneid,aa,by.x = 1, by.y = "Gene_Name")
        gene_id<-as.matrix(gene_id)
        AT<-cbind(gene_id[grep("AT", gene_id[,"Type"]), "Trascript_ID"])
        TI<-cbind(gene_id[grep("TI", gene_id[,"Type"]), "Trascript_ID"])
        ################################
        #AT<-as.array(Tdata[,list1])
        #TI<-as.array(Tdata[,list2])
        venn.plot <- reactive({venn.diagram(
          x = list(
            "AT" = AT,
            "TI" = TI
          ),
          main = "",
          filename =outfile, output=TRUE,
          lty = 3,na = "remove",imagetype="png",
          col="transparent",
          fill = c("cornflowerblue", "pink"),
          alpha = c(0.5,0.5),
          label.col = "black",
          cex=1.5,
          fontface = "plain",
          cat.col = c("black", "black"),
          cat.cex = 1.5,
          cat.fontfamily = "serif",
          cat.fontface = "plain",
          cat.dist = c(0.05, 0.05),
          cat.pos = c(-20, 14),
          cat.default.pos = "text",
          scaled = TRUE
        )
        })
        outfile <- tempfile(fileext = '.png')
        png(outfile, width = 500, height = 500,type="cairo")
        venn.plot()
        dev.off()
        
        list(src = outfile,
             contentType = 'image/png',
             width = 500,
             height = 500,
             alt = "This is alternate text")
      }}}
  }, deleteFile = TRUE)
  
  
  ################################################################ Transcript Table
  output$mytable12 <- DT::renderDataTable({
    shiny::validate(
      need(input$show_vars12, "")
    )
    if (input$Adv == FALSE&&input$pas == FALSE) {
    ####################################################################
      TMdata <- Tmedian()
      TMdata<-(as.matrix(TMdata))
      TMdata<-trimws(TMdata, "l") #remove space
      list<-c("Gene ID","Ensembl Gene ID","Ensembl Transcript ID",input$show_vars12)
      list2<-c("Gene ID","Ensembl Gene ID","Ensembl Transcript ID",input$show_vars15)
      acti<-TMdata[,list]
      tiss<-TMdata[,list2]
      ACT<-as.data.frame(acti)
      ACT$Type <- ifelse(rowSums(ACT[-1:-3]>=1.5), 'AT', '')
      TOI<-as.data.frame(tiss)
      TOI$Type <- ifelse(rowSums(TOI[-1:-3]>=1.5), 'TI', '')
      trans_table<-merge(ACT,TOI, by = c("Gene ID","Ensembl Gene ID","Ensembl Transcript ID"))%>% tidyr::unite(Type, Type.x, Type.y, sep = ' ', na.rm = TRUE)
      trans_table<-trans_table%>% dplyr::filter(!(Type==" ")) #remove empty colZumns
    ####################################################################
    
    Tdata <- Tdata()
    checkdata<-fread("transcript/tpm_geneID",header = T,sep = "\t")
    Tdata<-as.matrix(Tdata)
    list1<-rbind(input$show_vars12)
    list2<-rbind(input$show_vars15)
    AT<-as.array(Tdata[,list1])
    TI<-as.array(Tdata[,list2])
    bb<-paste((t(AT)))
    
    TI<-merge(TI,checkdata,by.x = 1, by.y = 1)
    AT<-merge(bb,checkdata,by.x = 1, by.y = 1)
    
    TI<-cbind(TI,Type='TI')
    AT<-cbind(AT,Type='AT')
    
    colnames(AT) <- c("Trascript_ID", "Gene_ID","Gene_Name","Type")
    colnames(TI) <- c("Trascript_ID", "Gene_ID","Gene_Name","Type")
    TI<-data.frame(TI)
    AT<-data.frame(AT)
    aa<-(dplyr::full_join(TI,AT, by = c('Gene_Name','Trascript_ID','Gene_ID')) %>% tidyr::unite(Type, Type.x, Type.y, sep = '/', na.rm = TRUE))
    output$mydownload05=downloadHandler(filename = "Selected_Results.csv",content = function(file){write.csv(aa,file)})
     DT::datatable((as.matrix(trans_table)),class = 'cell-border stripe',
                  caption = "List of Transcripts with Median Transcripts-level TPM by tissue",options = list(
                    paging = TRUE,
                    pageLength = 5,
                    columnDefs = list(list(className = "dt-center", targets = "_all"))
                  ))}
    else{
      if(input$pas == TRUE&&(input$Adv == TRUE))
    {
      Tdata <- Tdata()
      checkdata<-fread("transcript/tpm_geneID",header = T,sep = "\t")
      Tdata<-as.matrix(Tdata)
      list1<-rbind(input$show_vars12)
      list2<-(input$show_vars15)
      AT<-as.array(Tdata[,list1])
      TI<-as.array(Tdata[,list2])
      bb<-paste((t(AT)))
      
      TI<-merge(TI,checkdata,by.x = 1, by.y = 1)
      AT<-merge(bb,checkdata,by.x = 1, by.y = 1)
      
      TI<-cbind(TI,Type='TI')
      AT<-cbind(AT,Type='AT')
      
      colnames(AT) <- c("Trascript_ID", "Gene_ID","Gene_Name","Type")
      colnames(TI) <- c("Trascript_ID", "Gene_ID","Gene_Name","Type")
      TI<-data.frame(TI)
      AT<-data.frame(AT)
      aa<-(dplyr::full_join(TI,AT, by = c('Gene_Name','Trascript_ID','Gene_ID')) %>% tidyr::unite(Type, Type.x, Type.y, sep = '/', na.rm = TRUE))
      ########################
      geneid=strsplit(input$paste,",| |\n")
      geneid<-unlist(geneid)
      checkdata<-fread("transcript/tpm_geneID",header = T,sep = "\t")
      gene_id<-merge(geneid,aa,by.x = 1, by.y = "Gene_Name")
      ################################
      
      output$mydownload05=downloadHandler(filename = "Selected_Results.csv",content = function(file){write.csv(gene_id,file)})
      DT::datatable((as.matrix(gene_id)),class = 'cell-border stripe',
                    caption = "List of Transcripts with Median Transcripts-level TPM by tissue",colnames =c("Gene Name","Transcript ID","Gene ID","Type"),options = list(
                      paging = TRUE,
                      pageLength = 5,
                      columnDefs = list(list(className = "dt-center", targets = "_all"))
                    ))
      }
      else{
        if(!is.null(input$file1))
        {
          Tdata <- Tdata()
          checkdata<-fread("transcript/tpm_geneID",header = T,sep = "\t")
          Tdata<-as.matrix(Tdata)
          list1<-rbind(input$show_vars12)
          list2<-(input$show_vars15)
          AT<-as.array(Tdata[,list1])
          TI<-as.array(Tdata[,list2])
          bb<-paste((t(AT)))
          
          TI<-merge(TI,checkdata,by.x = 1, by.y = 1)
          AT<-merge(bb,checkdata,by.x = 1, by.y = 1)
          
          TI<-cbind(TI,Type='TI')
          AT<-cbind(AT,Type='AT')
          
          colnames(AT) <- c("Trascript_ID", "Gene_ID","Gene_Name","Type")
          colnames(TI) <- c("Trascript_ID", "Gene_ID","Gene_Name","Type")
          TI<-data.frame(TI)
          AT<-data.frame(AT)
          aa<-(dplyr::full_join(TI,AT, by = c('Gene_Name','Trascript_ID','Gene_ID')) %>% tidyr::unite(Type, Type.x, Type.y, sep = '/', na.rm = TRUE))
          ########################
          #df <- read.csv(input$file1$datapath,header = F)
          geneid <- unique(rv$df[, 1])
          checkdata<-fread("transcript/tpm_geneID",header = T,sep = "\t")
          gene_id<-merge(geneid,aa,by.x = 1, by.y = "Gene_Name")
          ################################
          
          output$mydownload05=downloadHandler(filename = "Selected_Results.csv",content = function(file){write.csv(gene_id,file)})
          DT::datatable((as.matrix(gene_id)),class = 'cell-border stripe',
                        caption = "List of Transcripts with Median Transcripts-level TPM by tissue",colnames =c("Gene Name","Transcript ID","Gene ID","Type"),options = list(
                          paging = TRUE,
                          pageLength = 5,
                          columnDefs = list(list(className = "dt-center", targets = "_all"))
                        ))
        }}}
  })
  
  
######################################################Single Gene Analysis Page###########################
  
  observeEvent(input$Gene_list,
               {
               data <- dataa()
               drop(data)
               data<-(data)
               selectList <- data$`Gene ID`
               
               updateSelectizeInput(session = session, inputId = 'mylist', choices = selectList, server = TRUE)})
  #tpm_data<-tpm_data()
  #test<-fread(cmd = paste("gunzip -cq", tpm_data ),header = T,sep = "\t")
  observeEvent(input$Gene_list,
               {
                 tpm_data<-tpm_data()
                 
                 #test<-fread(cmd = paste("gunzip -cq", tpm_data ),header = T,sep = "\t")
                 output$plot7 <- renderPlot({
                   
                   if (input$Adv == FALSE&&input$pas == FALSE) {
                     shiny::validate(need(input$show_vars4, "Select any one Tissue name"))
                     shiny::validate( need(input$mylist, "Select any one Gene name"))
                  Sample<-c(input$show_vars4)
                   #adding the combine the user inputed sample name and gene list
                   ###grep the user inputed genes
                   withProgress(message = 'Plotting in progress',
                                detail = 'Please wait...', value = 0, {
                                  for (i in 1:2) {
                                    incProgress(1/2)
                                    
                                    Gene_Name<-c(input$mylist) ####gene id 
                                    #list<-cbind(Gene_Name,Sample)
                                    list<-CJ(Gene_Name, Sample, unique = TRUE)
                                    vilolin_data<-merge(list,tpm_data,by=c("Gene_Name","Sample")) 
                                    vilolin_data$Gene_Name<- as.factor(vilolin_data$Gene_Name)
                                  }
                                  })

                   plot<-ggplot(vilolin_data,aes(x=Gene_Name,y=TPM,fill=Sample))
                   plot+geom_violin(aes(color = Sample), trim = FALSE,position = position_dodge(0.9) )+xlab("Gene Name") +
                     ylab("TPM")+geom_boxplot(aes(color = Sample), width = 0.15,position = position_dodge(0.9))
                   }
                   else
                   {
                     if(input$pas == TRUE&&(input$Adv == TRUE))
                   {
                       geneid=strsplit(input$paste,",| |\n")
                       geneid<-unlist(geneid)
                       #####################################################################################
                       data <- dataa()
                       data<-(as.matrix(data))
                       all_gtex_gens<-(data[,"Gene ID"])
                       input_genes<-intersect(all_gtex_gens,geneid)
                       #####################################################################################
                       geneid_zip<-lapply(input_genes,function(x)paste(x,".zip",sep = "")) ##adding zip extension 
                       geneid_zip_path<-lapply(geneid_zip,function(x)paste("TPM/",x,sep = "")) #Adding path 
                       all_tmp<-vroom(geneid_zip_path)
                       shiny::validate(need(input$show_vars4, "Select any one Tissue name"))
                       #shiny::validate( need(input$mylist, "Select any one Gene name"))
                       
                       Sample<-c(input$show_vars4)
                       #adding the combine the user inputed sample name and gene list
                       ###grep the user inputed genes
                       withProgress(message = 'Plotting in progress',
                                    detail = 'Please wait...', value = 0, {
                                      for (i in 1:2) {
                                        incProgress(1/2)
                                        
                                        Gene_Name<-c(geneid) ####gene id 
                                        #list<-cbind(Gene_Name,Sample)
                                        list<-CJ(Gene_Name, Sample, unique = TRUE)
                                        vilolin_data<-merge(list,all_tmp,by=c("Gene_Name","Sample")) 
                                        vilolin_data$Gene_Name<- as.factor(vilolin_data$Gene_Name)
                                      }
                                    })
                       plot<-ggplot(vilolin_data,aes(x=Gene_Name,y=TPM,fill=Sample))
                       plot+geom_violin(aes(color = Sample), trim = FALSE,position = position_dodge(0.9) )+xlab("Gene Name") +
                         ylab("TPM")+geom_boxplot(aes(color = Sample), width = 0.15,position = position_dodge(0.9))
                     }
                     else{
                       if(!is.null(input$file1))
                       {
                         geneid <- unique(rv$df[, 1])
                         ####################################################################################
                         data <- dataa()
                         data<-(as.matrix(data))
                         all_gtex_gens<-(data[,"Gene ID"])
                         input_genes<-intersect(all_gtex_gens,geneid)
                         ####################################################################################
                         geneid_zip<-lapply(input_genes,function(x)paste(x,".zip",sep = "")) ##adding zip extension 
                         geneid_zip_path<-lapply(geneid_zip,function(x)paste("TPM/",x,sep = "")) #Adding path 
                         all_tmp<-vroom(geneid_zip_path)
                         shiny::validate(need(input$show_vars4, "Select any one Tissue name"))
                         #shiny::validate( need(input$mylist, "Select any one Gene name"))
                         
                         Sample<-c(input$show_vars4)
                         #adding the combine the user inputed sample name and gene list
                         ###grep the user inputed genes
                         withProgress(message = 'Plotting in progress',
                                      detail = 'Please wait...', value = 0, {
                                        for (i in 1:2) {
                                          incProgress(1/2)
                                          
                                          Gene_Name<-c(geneid) ####gene id 
                                          #list<-cbind(Gene_Name,Sample)
                                          list<-CJ(Gene_Name, Sample, unique = TRUE)
                                          vilolin_data<-merge(list,all_tmp,by=c("Gene_Name","Sample")) 
                                          vilolin_data$Gene_Name<- as.factor(vilolin_data$Gene_Name)
                                        }
                                      })
                         
                         plot<-ggplot(vilolin_data,aes(x=Gene_Name,y=TPM,fill=Sample))
                         plot+geom_violin(aes(color = Sample), trim = FALSE,position = position_dodge(0.9) )+xlab("Gene Name") +
                           ylab("TPM")+geom_boxplot(aes(color = Sample), width = 0.15,position = position_dodge(0.9))
                       }
                 }}})
                 })
####################################################Single Gene Table #################################################################
  output$table05 <-DT::renderDataTable({
    
    ######################################
    
    if (input$Adv == FALSE&&input$pas == FALSE)
    {
      shiny::validate(need(input$show_vars4,"Select any one Tissue name"))
      shiny::validate(need(input$mylist, "Select any one Gene name"))
      Sample<-c(input$show_vars4)
      Gene_Name<-c(input$mylist)
      tpm_data<-tpm_data()
      list<-CJ(Gene_Name, Sample, unique = TRUE)
      table_data<-merge(list,tpm_data,by=c("Gene_Name","Sample"))
      datatable(table_data,class = 'cell-border stripe',colnames =c("Gene Name","Tissue","TPM"),
                    caption = HTML("Data Source: GTEx Analysis Release V8 <br/>
Displayed numbers represent gene-level TPM by tissue"),rownames= FALSE
      )
    }
    else{    
      if(input$pas == TRUE&&(input$Adv == TRUE))
      {
        geneid=strsplit(input$paste,",| |\n")
        geneid<-unlist(geneid)
        ###########################################################################
        data <- dataa()
        data<-(as.matrix(data))
        all_gtex_gens<-(data[,"Gene ID"])
        input_genes<-intersect(all_gtex_gens,geneid)
        
        ##########################################################################
        shiny::validate(need(input$show_vars4,"Select any one Tissue name"))
        geneid_zip<-lapply(input_genes,function(x)paste(x,".zip",sep = "")) ##adding zip extension 
        geneid_zip_path<-lapply(geneid_zip,function(x)paste("TPM/",x,sep = "")) #Adding path 
        all_tmp<-vroom(geneid_zip_path)
        Sample<-c(input$show_vars4)
        Gene_Name<-c(geneid)
        list<-CJ(Gene_Name, Sample, unique = TRUE)
        table_data<-merge(list,all_tmp,by=c("Gene_Name","Sample"))
        datatable(table_data,class = 'cell-border stripe',colnames =c("Gene Name","Tissue","TPM"),
                  caption = HTML("Data Source: GTEx Analysis Release V8 <br/>
Displayed numbers represent gene-level TPM by tissue"),rownames= FALSE
        )
        
      }
      else{
        if(!is.null(input$file1))
        {
          shiny::validate(need(input$show_vars4,"Select any one Tissue name"))
          geneid <- unique(rv$df[, 1])
          ############################check whether the inputted genes in Gtex
          data <- dataa()
          data<-(as.matrix(data))
          all_gtex_gens<-(data[,"Gene ID"])
          input_genes<-intersect(all_gtex_gens,geneid)
          ##########################################################################
          geneid_zip<-lapply(input_genes,function(x)paste(x,".zip",sep = "")) ##adding zip extension 
          geneid_zip_path<-lapply(geneid_zip,function(x)paste("TPM/",x,sep = "")) #Adding path 
          all_tmp<-vroom(geneid_zip_path)
          Sample<-c(input$show_vars4)
          Gene_Name<-c(geneid)
          list<-CJ(Gene_Name, Sample, unique = TRUE)
          table_data<-merge(list,all_tmp,by=c("Gene_Name","Sample"))
          datatable(table_data,class = 'cell-border stripe',colnames =c("Gene Name","Tissue","TPM"),
                    caption = HTML("Data Source: GTEx Analysis Release V8 <br/>
Displayed numbers represent gene-level TPM by tissue"),rownames= FALSE
          )
       
        }
      }}
  })
  
#########################################################################################################################################################
  
  #####download data table
  #output$mydownload1=downloadHandler(filename = "Selected_Results.csv",content = function(file){ write.csv(data[, input$show_vars1, drop = FALSE],file)})
 # output$mydownload2=downloadHandler(filename = "Correlation_Results.csv",content = function(file){write.csv(cor(data[,input$show_vars2]),file) })
  
  
  ####download plot1
  # downloadHandler contains 2 arguments as functions, namely filename, content
  
  ####download plot2
  # downloadHandler contains 2 arguments as functions, namely filename, content

  ####interactive plot gene page
  
}

shinyApp(ui, server)

