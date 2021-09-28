#PTEE is an application deigned to help clinicians decide which is the most appropriate tissue to inquire gene expression given a specific phenotype of the patient.#
#PTEE uses GTEx expression data (mean TPMs of genes and transcripts)
#Version:
#Contact akhilvbioinfo@gmail.com or diana.leduc@gmail.com for further infrmation or support.
######################################################################################################################################################################
####################
#Load all libraries#
####################
library(shinyBS)
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
######################################################################################################################################################################
list_values <- c(1:57)
names(list_values) <- c("Brain Amygdala", "Brain Anterior cingulate cortex (BA24)", "Brain Caudate (basal ganglia)","Brain Cerebellar Hemisphere",
                        "Brain Cerebellum","Brain Cortex","Brain Frontal Cortex (BA9)","Brain Hippocampus","Brain Hypothalamus","Brain Nucleus accumbens (basal ganglia)",
                        "Brain Putamen (basal ganglia)","Brain Spinal cord (cervical c-1)","Brain Substantia nigra","Adipose Subcutaneous", "Adipose Visceral (Omentum)",
                        "Adrenal Gland", "Pituitary", "Thyroid","Pancreas","Artery Aorta", "Artery Coronary", "Artery Tibial","Bladder", "Ovary", "Testis","Prostate","Fallopian Tube","Cervix Ectocervix","Cervix Endocervix","Uterus",
                        "Vagina","Kidney Cortex","Kidney Medulla","Cells EBV transformed lymphocytes","Cells Cultured fibroblasts","Breast Mammary Tissue",
                        "Colon Sigmoid", "Colon Transverse", "Esophagus Gastroesophageal Junction","Esophagus Mucosa",
                        "Esophagus Muscularis","Minor Salivary Gland","Pancreas","Stomach","Liver","Small Intestine Terminal Ileum",
                        "Heart Atrial Appendage", "Heart Left Ventricle","Lung","Muscle Skeletal","Nerve Tibial","Spleen","Cells EBV transformed lymphocytes","Cells Cultured fibroblasts"
                        ,"Skin Not Sun Exposed Suprapubic", "Skin Sun Exposed Lower leg","Whole Blood")

# 2. Index of selected element
index_selected <- 1

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

##############################
#User interface startig point#
##############################

ui <-  fluidPage(
  
  ######################******Logos and hyperlinks*****######################
  
  dbHeader <- dashboardHeader(
    tags$li(a(href = 'https://bioinf.eva.mpg.de/PTEE/',
              img(src = 'PTEE.png',
                  title = "Phenotype-Tissue Expression & Exploration", height = "70px"),
              style = "padding-top:5px; padding-bottom:5px;"),
            class = "dropdown")),
  
  
  useShinyjs(),
  tags$head(includeHTML(("google-analytics.html"))),
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
                                 "NDD Genes from SysID (Primary ID genes)" = "NDD Genes from SysID",
                                 "Cardiac Arrythmia" = "Cardiac Arrythmia","Obesity" = "Obesity",
                                 `Phenomizer Genes` = list("Abdomen Abnormality	HP:0001438"="Abdomen Abnormality",
                                                           "Blood Abnormality HP:0001871"="Blood Abnormality",
                                                           "Breast Abnormality HP:0000769"="Breast Abnormality",
                                                           "Cardiovascular Abnormality HP:0001626"="Cardiovascular Abnormality",
                                                           "Connective Tissue Abnormality HP:0003549"="Connective Tissue Abnormality",
                                                           "Ear Abbnormality HP:0000598"="Ear Abbnormality",
                                                           "Endocrine Abnormality HP:0000818"="Endocrine Abnormality",
                                                           "Eye Abnormality HP:0000478"="Eye Abnormality",
                                                           "Genitourinary Abnormality HP:0000119"="Genitourinary Abnormality",
                                                           "Growth Abnormality HP:0001507"="Growth Abnormality",
                                                           "Head Neck Abnormality HP:0000152"="Head Neck Abnormality",
                                                           "Immune Abnormality HP:0002715"="Immune Abnormality",
                                                           "Integument Abbnormality HP:0001574"="Integument Abbnormality",
                                                           "Limbs Abnormality HP:0040064"="Limbs Abnormality",
                                                           "Metabolism Abnormality HP:0001939"="Metabolism Abnormality",
                                                           "Musculature Abnormality HP:0003011"="Musculature Abnormality",
                                                           "Neoplasm HP:0002664"="Neoplasm",
                                                           "Nervous System Abnormality HP:0000707"="Nervous System Abnormality",
                                                           "Prenatal Abnormality HP:0001197"="Prenatal Abnormality",
                                                           "Respiratory Abnormality HP:0002086"="Respiratory Abnormality",
                                                           "Skeletal Abnormality HP:0000924"="Skeletal Abnormality",
                                                           "Thoracic Abnormality HP:0045027"="Thoracic Abnormality",
                                                           "Voice Abnormality HP:0001608"="Voice Abnormality")),selected = c("Cardiac Arrythmia")),
                   selectizeInput("show_vars5", "Tissue of Interest",
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
                                  multiple = TRUE,selected = c("Heart Atrial Appendage","Heart Left Ventricle"),
                                  options = list(
                                    onInitialize = I(onInitialize)
                                  )
                   ),
                   selectizeInput("show_vars2", "Tissue of Analysis",
                                  list(`Accessible Tissues` = list("Skin Not Sun Exposed Suprapubic", "Skin Sun Exposed Lower leg",'Whole Blood',"Muscle Skeletal",
                                                                   "Cells EBV transformed lymphocytes","Cells Cultured fibroblasts"),
                                       `All Other Tissues` = list("Heart Atrial Appendage", "Heart Left Ventricle","Brain Amygdala", "Brain Anterior cingulate cortex (BA24)", "Brain Caudate (basal ganglia)","Brain Cerebellar Hemisphere",
                                                                  "Brain Cerebellum","Brain Cortex","Brain Frontal Cortex (BA9)","Brain Hippocampus","Brain Hypothalamus","Brain Nucleus accumbens (basal ganglia)",
                                                                  "Brain Putamen (basal ganglia)","Brain Spinal cord (cervical c-1)","Brain Substantia nigra","Adipose Subcutaneous", "Adipose Visceral (Omentum)","Adrenal Gland", "Pituitary", "Thyroid","Pancreas",
                                                                  "Artery Aorta", "Artery Coronary", "Artery Tibial","Bladder", "Ovary", "Testis","Prostate","Fallopian Tube","Cervix Ectocervix","Cervix Endocervix","Uterus",
                                                                  "Vagina","Kidney Cortex","Kidney Medulla","Breast Mammary Tissue","Colon Sigmoid", "Colon Transverse", "Esophagus Gastroesophageal Junction","Esophagus Mucosa",
                                                                  "Esophagus Muscularis","Minor Salivary Gland","Pancreas","Stomach","Liver","Small Intestine Terminal Ileum","Lung","Nerve Tibial")
                                       
                                  ),
                                  multiple = TRUE,selected = c('Whole Blood',"Muscle Skeletal","Skin Not Sun Exposed Suprapubic", "Skin Sun Exposed Lower leg",
                                                               "Cells EBV transformed lymphocytes","Cells Cultured fibroblasts"),
                                  options = list(
                                    onInitialize = I(onInitialize)
                                  )
                   ),
                   actionButton("reset", "Reset")
                   ,tags$hr(),
                   checkboxInput(inputId = "Adv",label = strong("Custom Gene List"),value = FALSE,width ='100%'),
                   fileInput("file1", "Choose Gene List File (Ensembl Gene ID or HGNC Symbol)",multiple = FALSE,accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                   checkboxInput(inputId = "pas",label = strong("OR"),value = FALSE,width ='100%'),
                   textAreaInput("paste", "Paste Gene ID",placeholder ="Paste comma or space separated Ensembl Gene ID or HGNC Symbol"),
                   
                   #extendShinyjs(text = jsResetCode),
                   actionButton('reset02', 'Reset'),
                   downloadLink('example', 'Download example data',class = "butt"), tags$head(tags$style(".butt{background-color} .butt{color: #bf7136;}")),
                   checkboxInput(inputId = "exp",label = strong("OR"),value = FALSE,width ='100%'),
                   selectizeInput("show_vars55", "Select all genes expressed in a specific tissue",
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
                                  multiple = FALSE,
                                  options = list(
                                    onInitialize = I(onInitialize)))
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
                                  multiple = T,selected = list_values[index_selected],
                                  options = list(onInitialize = I(onInitialize)
                                                 
                                  )
                   ),
                   selectizeInput("show_vars33", "Tissue of Analysis (TA)",
                                  list(`Accessible Tissues` = list("Skin Not Sun Exposed Suprapubic", "Skin Sun Exposed Lower leg",'Whole Blood',"Muscle Skeletal",
                                                                   "Cells EBV transformed lymphocytes","Cells Cultured fibroblasts"),
                                       `All Other Tissues` = list("Heart Atrial Appendage", "Heart Left Ventricle","Brain Amygdala", "Brain Anterior cingulate cortex (BA24)", "Brain Caudate (basal ganglia)","Brain Cerebellar Hemisphere",
                                                                  "Brain Cerebellum","Brain Cortex","Brain Frontal Cortex (BA9)","Brain Hippocampus","Brain Hypothalamus","Brain Nucleus accumbens (basal ganglia)",
                                                                  "Brain Putamen (basal ganglia)","Brain Spinal cord (cervical c-1)","Brain Substantia nigra","Adipose Subcutaneous", "Adipose Visceral (Omentum)","Adrenal Gland", "Pituitary", "Thyroid","Pancreas",
                                                                  "Artery Aorta", "Artery Coronary", "Artery Tibial","Bladder", "Ovary", "Testis","Prostate","Fallopian Tube","Cervix Ectocervix","Cervix Endocervix","Uterus",
                                                                  "Vagina","Kidney Cortex","Kidney Medulla","Breast Mammary Tissue","Colon Sigmoid", "Colon Transverse", "Esophagus Gastroesophageal Junction","Esophagus Mucosa",
                                                                  "Esophagus Muscularis","Minor Salivary Gland","Pancreas","Stomach","Liver","Small Intestine Terminal Ileum","Lung","Nerve Tibial")
                                       
                                  )
                                  ,
                                  multiple = TRUE,selected =list_values[index_selected],
                                  options = list(onInitialize = I(onInitialize)
                                                 
                                  )
                   ),
                   radioButtons(inputId = "var10", label = "Select file type for the plot", choices = list("png", "pdf"))
                   
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
                                  multiple = T,selected = list_values[index_selected],
                                  options = list(onInitialize = I(onInitialize)
                                                 
                                  )
                   ),
                   selectizeInput("show_vars23", "Tissue of Analysis (TA)",
                                  list(`Accessible Tissues` = list("Skin Not Sun Exposed Suprapubic", "Skin Sun Exposed Lower leg",'Whole Blood',"Muscle Skeletal",
                                                                   "Cells EBV transformed lymphocytes","Cells Cultured fibroblasts"),
                                       `All Other Tissues` = list("Heart Atrial Appendage", "Heart Left Ventricle","Brain Amygdala", "Brain Anterior cingulate cortex (BA24)", "Brain Caudate (basal ganglia)","Brain Cerebellar Hemisphere",
                                                                  "Brain Cerebellum","Brain Cortex","Brain Frontal Cortex (BA9)","Brain Hippocampus","Brain Hypothalamus","Brain Nucleus accumbens (basal ganglia)",
                                                                  "Brain Putamen (basal ganglia)","Brain Spinal cord (cervical c-1)","Brain Substantia nigra","Adipose Subcutaneous", "Adipose Visceral (Omentum)","Adrenal Gland", "Pituitary", "Thyroid","Pancreas",
                                                                  "Artery Aorta", "Artery Coronary", "Artery Tibial","Bladder", "Ovary", "Testis","Prostate","Fallopian Tube","Cervix Ectocervix","Cervix Endocervix","Uterus",
                                                                  "Vagina","Kidney Cortex","Kidney Medulla","Breast Mammary Tissue","Colon Sigmoid", "Colon Transverse", "Esophagus Gastroesophageal Junction","Esophagus Mucosa",
                                                                  "Esophagus Muscularis","Minor Salivary Gland","Pancreas","Stomach","Liver","Small Intestine Terminal Ileum","Lung","Nerve Tibial")),
                                  multiple = TRUE,selected = list_values[index_selected],
                                  options = list(onInitialize = I(onInitialize)
                                                 
                                  ),
                   ),
                   helpText(strong("Venn Diagram of Expresed Genes"),br()),textOutput("selected_Gene_list"),
                   textOutput("selected_show_vars23"),
                   textOutput("selected_show_vars22"),
                   imageOutput("myImagevenn")%>% withSpinner(color="#0dc5c1"),
                   downloadButton(outputId = "down4", label = "")),
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
                                                                      `Spleen` = list("Spleen"),
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
                 conditionalPanel(
                   'input.dataset === "About"',
                   helpText("PTTE Version 1.2 (Updated on September 2021)",br(),
                            "Please refer to the manual for detailed guidance"),
                   downloadLink("downloadData", "Manual"),
                   
                   
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
                                  multiple = T,selected = list_values[index_selected],
                                  options = list(onInitialize = I(onInitialize)
                                                 
                                  )
                   ),
                   selectizeInput("show_vars12", "Tissue of Analysis (TA)",
                                  list(`Accessible Tissues` = list("Skin Not Sun Exposed Suprapubic", "Skin Sun Exposed Lower leg",'Whole Blood',"Muscle Skeletal",
                                                                   "Cells EBV transformed lymphocytes","Cells Cultured fibroblasts"),
                                       `All Other Tissues` = list("Heart Atrial Appendage", "Heart Left Ventricle","Brain Amygdala", "Brain Anterior cingulate cortex (BA24)", "Brain Caudate (basal ganglia)","Brain Cerebellar Hemisphere",
                                                                  "Brain Cerebellum","Brain Cortex","Brain Frontal Cortex (BA9)","Brain Hippocampus","Brain Hypothalamus","Brain Nucleus accumbens (basal ganglia)",
                                                                  "Brain Putamen (basal ganglia)","Brain Spinal cord (cervical c-1)","Brain Substantia nigra","Adipose Subcutaneous", "Adipose Visceral (Omentum)","Adrenal Gland", "Pituitary", "Thyroid","Pancreas",
                                                                  "Artery Aorta", "Artery Coronary", "Artery Tibial","Bladder", "Ovary", "Testis","Prostate","Fallopian Tube","Cervix Ectocervix","Cervix Endocervix","Uterus",
                                                                  "Vagina","Kidney Cortex","Kidney Medulla","Breast Mammary Tissue","Colon Sigmoid", "Colon Transverse", "Esophagus Gastroesophageal Junction","Esophagus Mucosa",
                                                                  "Esophagus Muscularis","Minor Salivary Gland","Pancreas","Stomach","Liver","Small Intestine Terminal Ileum","Lung","Nerve Tibial")),
                                  multiple = TRUE,selected = list_values[index_selected],
                                  options = list(onInitialize = I(onInitialize)
                                                 
                                  )
                   ),
                   helpText(strong("Venn Diagram of Expresed Transcripts")),textOutput("selected_show_vars12"),textOutput("selected_show_vars15"),
                   imageOutput("Vennout")%>% withSpinner(color="#0dc5c1"),
                   downloadButton(outputId = "down5", label = ""),)),
    ######################################################################################################################################################################
    
    
    #####################*****Main panel Design*****#####################
    mainPanel(
      tabsetPanel(
        id = 'dataset',
        tabPanel("Choose Phenotype",bordered = F,imageOutput("myImage")%>% withSpinner(color="#0dc5c1"),htmlOutput("not_list"),DT::dataTableOutput("table1")%>% withSpinner(color="#0dc5c1"),downloadButton(outputId = "mydownload123",label = "")),
        tabPanel("Correlation Analysis",id ="Correlation Analysis",DT::dataTableOutput("mytable2")%>% withSpinner(color="#0dc5c1"),uiOutput("mydownload2"),
                 plotOutput("plot1", width = 800, height = 600)%>% withSpinner(color="#0dc5c1"),uiOutput("down2"),
                 checkboxInput(inputId = "pvalue",label = strong("Calculate P-Value from the Exact Binomial Test"),value = FALSE,width ='100%'),DT::dataTableOutput("finalloop")%>% withSpinner(color="#0dc5c1"),
                 #DT::dataTableOutput("final")%>% withSpinner(color="#0dc5c1"),
                 #plotOutput("plot5", width = 800, height = 600),uiOutput("down3")
        ),
        tabPanel("Expression Analysis",bordered = F,plotOutput("geneexp")%>% withSpinner(color="#0dc5c1"),
                 DT::dataTableOutput("mytable20")%>% withSpinner(color="#0dc5c1"),downloadButton(outputId = "mydownload20",label = "")),
        tabPanel("Transcript Analysis",plotOutput("tranexp")%>% withSpinner(color="#0dc5c1"),DT::dataTableOutput("mytable12")%>% withSpinner(color="#0dc5c1"),downloadButton(outputId = "mydownload05",label = "")),
        tabPanel("Single Gene Analysis",textOutput("selected_var"),plotOutput("plot7", width = 1000, height = 600)%>% withSpinner(color="#0dc5c1"),uiOutput("down6"),textOutput("selected_var2"),DT::dataTableOutput("table05")%>% withSpinner(color="#0dc5c1"),uiOutput("down7"))
        ,
        #tabPanel(title = "Conclusion", strong("The results as follws"),p("Based on the GE analysis the best Accessible tissue for the XXXXX and phenotype XXXXXXXXX")),
        tabPanel(
          title = "About",
          strong("Description"),
          p("Phenotype-Tissue Expression & Exploration is a web based tool designed to help clinical geneticists decide which tissue is best suited for RNA-seq analysis. Based on the human phenotype ontology (HPO) annotation the clinician can choose which phenotypical abnormality best matches the patient. This is linked to a tissue of interest (e.g. heart for HP:0001626 abnormality of the cardiovascular system) that can further be selected by the clinician. In the subsequent analyses only genes that are annotated to the chosen HPO will be included. The clinician can then choose six usually accessible tissues (whole blood, sun exposed skin, not sun exposed skin, muscle, cultured lymphocytes – Epstein-Barr virus (EBV) transformation, and cultured fibroblasts or other tissues."),
          strong("Correlation Analysis"),
          p("This analysis shows the degree of correlation based on genes expressed (median Transcripts Per Kilobase Million (TPM) > 1.5) in the tissue of analysis vs. tissue of interest for genes annotated in the selected HPO."),
          strong("Expression Analysiss"),
          p("This analysis shows the degree of intersection between genes expressed (median TPM > 1.5) in the tissue of analysis vs. tissue of interest for genes annotated in the selected HPO. For a chosen gene you can visualize the expression level in each tissue."),
          strong("Transcript Analysis"),
          p("This analysis shows the degree of intersection between transcripts expressed (median TPM > 1.5) in the tissue of analysis vs. tissue of interest for genes annotated in the selected HPO. For a chosen gene you can visualize which transcripts are expressed in each tissue."),
          strong("Single Gene Analysis"),
          p("For a chosen gene you can visualize the TPM values in the tissue of interest and the selected tissue of analysis."),
          #strong("Conclusion"),p("On this page we will infer which accessible tissue best matches expression data of the tissue of interest based on the chosen phenotype."),
          br(),
          p(em("This tool is designed on R-Shiny package and the data from Genotype-Tissue Expression (GTEx) project (https://gtexportal.org/home/)")),
          icon = icon("question-circle"))
        
      )
    )
  )
  ,bsTooltip("Gene_list", "Choose a phenotype based on patient‘s symptoms", placement = "right", trigger = "hover",
             options = NULL),
  bsTooltip("show_vars5", "Choose one or more tissues of interest based on the inferred pathophysiology", placement = "right", trigger = "hover",
            options = NULL),
  bsTooltip("show_vars2", "Choose one or more tissues for the analysis", placement = "right", trigger = "hover",
            options = NULL),
  bsTooltip("reset", "Deselect TI and TA", placement = "bottom", trigger = "hover",
            options = NULL),
  bsTooltip("downloadData", "Download the user manual", placement = "bottom", trigger = "hover",
            options = NULL),
  bsTooltip("Adv", "Upload gene list", placement = "bottom", trigger = "hover",
            options = NULL),
  #bsTooltip("dataset", "Overlap of transcripts corresponding to genes included in the inquired gene list", placement = "bottom", trigger = "hover",options = NULL)
  
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
    
    #####################*****Custom Gene List Functions*****#####################
    
    if((input$Adv == TRUE)) {
      shinyjs::show(id = "file1")
      shinyjs::show(id = "reset02")
      #shinyjs::show(id = "downloadData")
      shinyjs::show(id = "example")
      shinyjs::show(id = "paste")
      shinyjs::show(id = "pas")
      shinyjs::disable("Gene_list")
      #shinyjs::disable("mylist")
      shinyjs::enable("file1")
      shinyjs::disable("paste")
      output$myImage <-renderImage({
        Leg<-"image/All_genes.png" 
        list(src=Leg)},deleteFile = FALSE)
      shinyjs::show(id = "show_vars55")
      shinyjs::show(id = "exp")
      
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
      shinyjs::hide(id = "exp")
      reset("pas")
      shinyjs::hide(id = "show_vars55")
    }
  })
  
  observe({
    
    if((input$pas == TRUE)) {
      reset("file1")
      shinyjs::disable("file1")
      shinyjs::enable("paste")
      shinyjs::disable("show_vars55")
      shinyjs::disable("exp")
      rv$df<-NULL
      
    }
    else {
      shinyjs::disable("paste")
      reset("paste")
      shinyjs::enable("file1")
      shinyjs::enable("show_vars55")
      shinyjs::enable("exp")
    }
    
    ###########################################################
    if((input$exp == TRUE)) {
      reset("file1")
      shinyjs::disable("file1")
      shinyjs::disable("paste")
      shinyjs::disable("pas")
      rv$df<-NULL
      shinyjs::enable("show_vars55")
      
    }
    else {
      shinyjs::enable("paste")
      reset("paste")
      shinyjs::enable("file1")
      shinyjs::disable("show_vars55")
      shinyjs::enable("pas")
      
    }
    
  })
  
  
  #######################################Selection Based on the First Tab#####################################################################################
  vals <- reactiveValues(sync = 1)
  vals2 <- reactiveValues(sync = 1)
  
  observe(
    {
      req(input$show_vars5)
      vals$sync <- input$show_vars5
    }
  )
  observe(
    {
      req(input$show_vars2)
      vals2$sync <- input$show_vars2
    })
  observe(
    {
      req(vals$sync)
      req(vals2$sync)
      updateSelectizeInput(session, 'show_vars32', selected = vals$sync)
      updateSelectizeInput(session, 'show_vars33', selected = vals2$sync)
      updateSelectizeInput(session, 'show_vars22', selected = vals$sync)
      updateSelectizeInput(session, 'show_vars23', selected = vals2$sync)
      updateSelectizeInput(session, 'show_vars15', selected = vals$sync)
      updateSelectizeInput(session, 'show_vars12', selected = vals2$sync)
    })
  
  ###################################################################################################################################################################### 
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
    if((input$Adv == TRUE)) 
    { fread('data/all.txt',sep = "\t",header = T,check.names = FALSE)
    }
    
    else{
      if((input$Gene_list == 'Cardiac Arrythmia')) 
      { fread('data/Cardiac-Arrythmia.txt',sep = "\t",header = T,check.names = FALSE)
      }
      else{
        if (input$Gene_list == 'Obesity')
        {
          fread('data/Obesity.txt',sep = "\t",header = T,check.names = FALSE)
        }
        else{
          if (input$Gene_list == 'NDD Genes from SysID')
          {
            fread('data/sysID.txt',sep = "\t",header = T,check.names = FALSE)
          }
          else{
            if (input$Gene_list == 'Blood Abnormality')
            {
              fread('data/Blood_abnormality.txt',sep = "\t",header = T,check.names = FALSE)
            }
            else{
              if (input$Gene_list == 'Breast Abnormality')
              {
                fread('data/Breast_abnormality.txt',sep = ",",header = T,check.names = FALSE)
              }
              else{
                if (input$Gene_list == 'Abdomen Abnormality')
                {
                  fread('data/Abdomen-abnormality.txt',sep = ",",header = T,check.names = FALSE)
                }
                else{
                  if (input$Gene_list == 'Cardiovascular Abnormality')
                  {
                    fread('data/Cardiovascular-abnormality.txt',sep = ",",header = T,check.names = FALSE)
                  }
                  else{
                    if (input$Gene_list == 'Connective Tissue Abnormality')
                    {
                      fread('data/Connective_tissue_abnormality.txt',sep = ",",header = T,check.names = FALSE)
                    }
                    else{
                      if (input$Gene_list == 'Ear Abbnormality')
                      {
                        fread('data/Ear-abbnormality.txt',sep = ",",header = T,check.names = FALSE)
                      }
                      else{
                        if (input$Gene_list == 'Endocrine Abnormality')
                        {
                          fread('data/Endocrine-abnormality.txt',sep = ",",header = T,check.names = FALSE)
                        }
                        else{
                          if (input$Gene_list == 'Eye Abnormality')
                          {
                            fread('data/Eye-abnormality.txt',sep = ",",header = T,check.names = FALSE)
                          }
                          else{
                            if (input$Gene_list == 'Genitourinary Abnormality')
                            {
                              fread('data/Genitourinary_abnormality.txt',sep = ",",header = T,check.names = FALSE)
                            }
                            else{
                              if (input$Gene_list == 'Growth Abnormality')
                              {
                                fread('data/Growth-abnormality.txt',sep = ",",header = T,check.names = FALSE)
                              }
                              else{
                                if (input$Gene_list == 'Head Neck Abnormality')
                                {
                                  fread('data/Head-neck-abnormality.txt',sep = ",",header = T,check.names = FALSE)
                                }
                                else{
                                  if (input$Gene_list == 'Immune Abnormality')
                                  {
                                    fread('data/Immune-abnormality.txt',sep = ",",header = T,check.names = FALSE)
                                  }
                                  else{
                                    if (input$Gene_list == 'Integument Abbnormality')
                                    {
                                      fread('data/Integument-abbnormality.txt',sep = ",",header = T,check.names = FALSE)
                                    }
                                    else{
                                      if (input$Gene_list == 'Limbs Abnormality')
                                      {
                                        fread('data/Limbs_abnormality.txt',sep = ",",header = T,check.names = FALSE)
                                      }
                                      else{
                                        if (input$Gene_list == 'Metabolism Abnormality')
                                        {
                                          fread('data/Metabbolism_abnormality.txt',sep = ",",header = T,check.names = FALSE)
                                        }
                                        else{
                                          if (input$Gene_list == 'Musculature Abnormality')
                                          {
                                            fread('data/Musculature-abnormality.txt',sep = ",",header = T,check.names = FALSE)
                                          }
                                          else{
                                            if (input$Gene_list == 'Neoplasm')
                                            {
                                              fread('data/Neoplasm.txt',sep = ",",header = T,check.names = FALSE)
                                            }
                                            else{
                                              if (input$Gene_list == 'Nervous System Abnormality')
                                              {
                                                fread('data/Nervous-system-abnormality.txt',sep = ",",header = T,check.names = FALSE)
                                              }
                                              else{
                                                if (input$Gene_list == 'Prenatal Abnormality')
                                                {
                                                  fread('data/Prenatal-abnormality.txt',sep = ",",header = T,check.names = FALSE)
                                                }
                                                else{
                                                  if (input$Gene_list == 'Respiratory Abnormality')
                                                  {
                                                    fread('data/Respiratory_abnormality.txt',sep = ",",header = T,check.names = FALSE)
                                                  }
                                                  else{
                                                    if (input$Gene_list == 'Skeletal Abnormality')
                                                    {
                                                      fread('data/Skeletal-abnormality.txt',sep = ",",header = T,check.names = FALSE)
                                                    }
                                                    else{
                                                      if (input$Gene_list == 'Thoracic Abnormality')
                                                      {
                                                        fread('data/Thoracic-abnormality.txt',sep = ",",header = T,check.names = FALSE)
                                                      }
                                                      else{
                                                        if (input$Gene_list == 'Voice Abnormality')
                                                        {
                                                          fread('data/Voice-abnormality.txt',sep = ",",header = T,check.names = FALSE)
                                                        }
                                                        else{
                                                          if (input$Gene_list == 'Abdomen Abnormality')
                                                          {
                                                            fread('data/Abdomen-abnormality.txt',sep = ",",header = T,check.names = FALSE)
                                                          }
                                                        }}}}}}}}}}}}}}}}}}}}}}}}}}}
    
  })
  
  #####################################################################################################################################################
  ###################Load Advance Gene Database########################################################################################################
  expdata<-reactive({
    if((input$Adv == TRUE&&input$exp == TRUE)) 
    {
      input_tissue <- (input$show_vars55)
      tissue_path<-lapply(input_tissue,function(x)paste("ExpData/",x,sep = "")) #Adding path 
      expdata<-vroom(tissue_path)
    }
  })
  ########################################################################################################################################################
  Traexpdata<-reactive({
    if((input$Adv == TRUE&&input$exp == TRUE)) 
    {
      input_tissue <- (input$show_vars55)
      tissue_path<-lapply(input_tissue,function(x)paste("TraExp/",x,sep = "")) #Adding path 
      expdata<-vroom(tissue_path)
    }
  })
  ########################################################################################################################################################
  
  #Phenotype Image on the first page
  ######################################################################################
  output$myImage <-renderImage({
    if (input$Gene_list == 'Cardiac Arrythmia') Leg<-"image/Cardiac_Arrhythmia.png"
    if (input$Gene_list == 'Blood Abnormality') Leg<-"image/Blood_abnormality.png"
    if (input$Gene_list == 'Breast Abnormality') Leg<-"image/Breast_abnormality.png"
    if (input$Gene_list == 'Abdomen Abnormality') Leg<-"image/Abdomen_abnormality.png"
    if (input$Gene_list == 'Cardiovascular Abnormality') Leg<-"image/Cardiovascular_abnormality.png"
    if (input$Gene_list == 'Connective Tissue Abnormality') Leg<-"image/Connective_tissue_abnormality.png"
    if (input$Gene_list == 'Ear Abbnormality') Leg<-"image/Ear_abnormality.png"
    if (input$Gene_list == 'Endocrine Abnormality') Leg<-"image/Endocrine_abnormality.png"
    if (input$Gene_list == 'Eye Abnormality') Leg<-"image/Eye_abnormality.png"
    if (input$Gene_list == 'Genitourinary Abnormality') Leg<-"image/Genitourinary_abnormality.png"
    if (input$Gene_list == 'Growth Abnormality') Leg<-"image/Growth_abnormality.png"
    if (input$Gene_list == 'Head Neck Abnormality') Leg<-"image/Head_neck_abnormality.png"
    if (input$Gene_list == 'Immune Abnormality') Leg<-"image/Immune_abnormality.png"
    if (input$Gene_list == 'Integument Abbnormality') Leg<-"image/Integument_abnormality.png"
    if (input$Gene_list == 'Limbs Abnormality') Leg<-"image/Limbs_abnormality.png"
    if (input$Gene_list == 'Metabolism Abnormality') Leg<-"image/Metabolism_abnormality.png"
    if (input$Gene_list == 'Musculature Abnormality') Leg<-"image/Musculature_abnormality.png"
    if (input$Gene_list == 'Neoplasm') Leg<-"image/Neoplasm.png"
    if (input$Gene_list == 'Nervous System Abnormality') Leg<-"image/Nervous_system_abnormality.png"
    if (input$Gene_list == 'Prenatal Abnormality') Leg<-"image/Prenatal_abnormality.png"
    if (input$Gene_list == 'Respiratory Abnormality') Leg<-"image/Respiratory_abnormality.png"
    if (input$Gene_list == 'Skeletal Abnormality') Leg<-"image/Skeletal_abnormality.png"
    if (input$Gene_list == 'Thoracic Abnormality') Leg<-"image/Thoracic_abnormality.png"
    if (input$Gene_list == 'Voice Abnormality') Leg<-"image/Voice_abnormality.png"
    if (input$Gene_list == 'NDD Genes from SysID') Leg<-"image/NDD.png"
    if (input$Gene_list == 'Obesity') Leg<-"image/Obesity.png"
    
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
    shiny::validate(need(input$show_vars2,"Enter one Tissue of Analysis"))
    shiny::validate(need(input$show_vars5,"Enter one Tissue of Interest"))
    ######################################
    
    if (input$Adv == FALSE&&input$pas == FALSE)
    {
      
      data <- dataa()
      data[, 3:ncol(data)] <- round(data[, 3:ncol(data)], 2)
      data<-(as.matrix(data))
      list<-c("Gene ID","Ensembl ID",input$show_vars2,input$show_vars5)
      table<-(data[,list])
      output$mydownload123=downloadHandler(filename = "Phenotype of Interest Table.csv",content = function(file){write.csv((data[,list]),file)})
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
        if(length(non_matched)>0)
        {
          output$not_list <- renderUI({HTML((paste("The Gene ID",non_matched,"was not found in the database","<br/>")))})
        }
        else{
          output$not_list <- renderUI({HTML((paste("","<br/>")))})
        }
        ######################################################################
        table<-data[data[, 1] %in% geneid,list]
        table<-as.matrix(table)
        output$mydownload123=downloadHandler(filename = "Phenotype of Interest Table.csv",content = function(file){write.csv((table),file)})
        DT::datatable(table,class = 'cell-border stripe',
                      caption = HTML("Data Source: GTEx Analysis Release V8 <br/>
Displayed numbers represent median gene-level TPM by tissue"),filter="top"
        )
        
      }
      else{
        if((input$Adv == TRUE)&& !is.null(input$file1))
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
          
          if(length(non_matched)>0)
          {
            output$not_list <- renderUI({HTML((paste("The Gene ID",non_matched,"was not found in the database","<br/>")))})
          }
          else{
            output$not_list <- renderUI({HTML((paste("","<br/>")))})
          }
          ######################################################################
          table<-data[data[, 1] %in% geneid,list]
          table<-as.matrix(table)
          output$mydownload123=downloadHandler(filename = "Phenotype of Interest Table.csv",content = function(file){write.csv((table),file)})
          DT::datatable(table,class = 'cell-border stripe',
                        caption = HTML("Data Source: GTEx Analysis Release V8 <br/>
Displayed numbers represent median gene-level TPM by tissue"),filter="top"
          )
          
        }
        else
        {if((input$Adv == TRUE)&& input$exp == TRUE)
        {
          data <- expdata()
          data[, 3:ncol(data)] <- round(data[, 3:ncol(data)], 2)
          data<-(as.matrix(data))
          list<-c("Gene ID","Ensembl ID",input$show_vars2,input$show_vars5)
          table<-(data[,list])
          output$mydownload123=downloadHandler(filename = "Phenotype of Interest Table.csv",content = function(file){write.csv((data[,list]),file)})
          datatable(table,class = 'cell-border stripe',
                    caption = HTML("Data Source: GTEx Analysis Release V8 <br/>
Displayed numbers represent median gene-level TPM by tissue"),)
        } 
          
        }}}
  })
  ###############################End of First Page#####################
  
  ############################################################ Start of  Second Page###################################################
  
  ######******Correlations calculations******##########################################################################################
  output$mytable2 <- DT::renderDataTable({
    shiny::validate(
      need(input$show_vars32,"Select a Tissue of Interest")
    )
    shiny::validate(
      need(input$show_vars33,"Select a Tissue of Analysis")
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
      output$mydownload2 <- renderUI({
        if(!is.null(list)) {
          downloadButton('table01', '')}})
      output$table01=downloadHandler(filename = "Correlation matrix.csv",content = function(file){write.csv(cor(data[,list]),file)})
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
        output$mydownload2 <- renderUI({
          if(!is.null(table)) {
            downloadButton('table02', '')}})
        
        output$table02=downloadHandler(filename = "Correlation matrix.csv",content = function(file){write.csv(cor(table),file)})
        DT::datatable(cor(table),class = 'cell-border stripe',
                      caption = "Correlation matrix")
      }
      else{
        if((input$Adv == TRUE)&& !is.null(input$file1))
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
          output$mydownload2 <- renderUI({
            if(!is.null(table)) {
              downloadButton('table03', '')}})
          output$table03=downloadHandler(filename = "Correlation matrix.csv",content = function(file){write.csv(cor(table),file)})
          DT::datatable(cor(table),class = 'cell-border stripe',
                        caption = "Correlation matrix")
        }
        else
        {
          if((input$Adv == TRUE)&& input$exp == TRUE)
          {
            dataa <- expdata()
            dataa[dataa <=1.5] <- 0
            drop(dataa)
            dataa$`Ensembl ID`<-NULL
            dataa$`Gene ID`<-NULL
            data<-as.matrix(dataa)
            list<-c(input$show_vars32,input$show_vars33)
            output$mydownload2 <- renderUI({
              if(!is.null(list)) {
                downloadButton('table01', '')}})
            output$table01=downloadHandler(filename = "Correlation matrix.csv",content = function(file){write.csv(cor(data[,list]),file)})
            DT::datatable(cor(data[,list]),class = 'cell-border stripe',
                          caption = "Correlation matrix")
          }
        }
      }}
  })
  
  #########********Corelation Plots*****##########
  vals <- reactiveValues()
  
  output$plot1 <- renderPlot({
    shiny::validate(
      need(input$show_vars32,"")
    )
    shiny::validate(
      need(input$show_vars33,"")
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
        scale_fill_gradient2(low="#91bfdb", mid="#f7f7f7", high="#fc8d59",midpoint = (min(plot_scale)+max(plot_scale))/2,limit=c(min(plot_scale),1)) +
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
          scale_fill_gradient2(low="#91bfdb", mid="#f7f7f7", high="#fc8d59",midpoint = (min(plot_scale)+max(plot_scale))/2,limit=c(min(plot_scale),1)) +
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
        if((input$Adv == TRUE)&& !is.null(input$file1))
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
            scale_fill_gradient2(low="#91bfdb", mid="#f7f7f7", high="#fc8d59",midpoint = (min(plot_scale)+max(plot_scale))/2,limit=c(min(plot_scale),1)) +
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
          if((input$Adv == TRUE)&& input$exp == TRUE)
          {
            dataa <- expdata()
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
              scale_fill_gradient2(low="#91bfdb", mid="#f7f7f7", high="#fc8d59",midpoint = (min(plot_scale)+max(plot_scale))/2,limit=c(min(plot_scale),1)) +
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
        }}}
  })
  #################################save plot01
  output$down2 <- renderUI({
    if(!is.null(vals$gg1)) {
      downloadButton('matrix01', '')
    }
  })
  output$matrix01 <- downloadHandler(
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
  
############################################################################################################################
####################################P value loop ###########################################################################
  output$finalloop <- DT::renderDataTable({
    
    if((input$pvalue == TRUE)) {
      
    if (input$Adv == FALSE&&input$pas == FALSE) { 
      ##########################################
      dataa <- dataa()
      dataa[dataa <=1.5] <- 0
      drop(dataa)
      dataa$`Ensembl ID`<-NULL
      dataa$`Gene ID`<-NULL
      data<-as.matrix(dataa)
      numbergene<-nrow(data)
      #list<-c(input$show_vars32,input$show_vars33)
      #target <-c(input$show_vars33)
      #plot_data<-cor(data[,list]) %>% as.data.frame() %>% mutate(var1 = rownames(.)) %>% gather(var2, value, -var1) %>% arrange(desc(value)) %>% group_by(value) %>%filter(row_number()==1)
      #'%ni%' <- Negate('%in%') 
      #filterlist<-filter(plot_data,var1 %in% target && var2 %ni% target)
      #bestone<-filterlist[filterlist$value==max(filterlist$value),1]
      
      #########################################
      nrun <- 100
      my_list <- list()
      alldata<-fread("data/dataall.txt",check.names = FALSE)
      alldata[alldata <=1.5] <- 0
      ldata<-as.matrix(alldata)
      list<-c("gene",input$show_vars32,input$show_vars33)
      fdata<-ldata[,list]
      for (i in 1:nrun) 
      {
        gene<-as.data.frame(sample((fdata[,"gene"]),`numbergene`))
        colnames(gene)<-c("gene")
        rundata<-merge(fdata,gene,by = "gene")
        rundata$gene<-NULL
        rundata$`Ensembl ID`<-NULL
        target <-c(input$show_vars33)

        plot_data<-cor(sapply(rundata, function(x) as.numeric(as.character(x))))%>% as.data.frame() %>% mutate(var1 = rownames(.)) %>% gather(var2, value, -var1) %>% arrange(desc(value)) %>% group_by(value) %>%filter(row_number()==1)
       '%ni%' <- Negate('%in%') 
        test<-filter(plot_data,var1 %in% target && var2 %ni% target)
        tisuue<-test[test$value==max(test$value),1]
        my_list[[i]] <- tisuue 
      }

       allout<-as.matrix(table(unlist(my_list)))
       #selectedlist<-allout[target,]
       selectedlist<-allout[rownames(allout) %in% target,] 
       se<-as.matrix(selectedlist)
       sortedm<-se[order(se[,1],decreasing=TRUE),]
       sortedm<-as.matrix(sortedm)
       if(nrow(se)==1){
         
         bt <- function(a) {binom.test(a, 100,p = 0.5, alternative=c("greater"), conf.level = 0.95)$p.value}
         btresult<-mapply(bt,selectedlist)
       }
       else{
         b<-sortedm[2,]
         bt <- function(a,b) {binom.test(a, 100,b, alternative=c("greater"), conf.level = 0.95)$p.value}
         btresult<-mapply(bt,selectedlist,(b/100))
       }
       
       
       
      #itcount<-sum(sapply(my_list, function(x)sum(grepl(bestone$var1, x))))
      #binomresults<-binom.test(`itcount`,100 , p = 0.5, alternative = c("greater"), conf.level = 0.95)
      #finalout<-cbind(bestone,itcount, binomresults$p.value)
      finalout<-cbind(btresult,selectedlist)
      DT::datatable(as.matrix(finalout),class = 'cell-border stripe',colnames=c("Tissue of Analysis","P-Value",
                                                                                "No. of highest correlations in 100 random iterations")
      )}
    else{
      if((input$Adv == TRUE)&& input$exp == TRUE)
      {
        dataa <- expdata()
        dataa[dataa <=1.5] <- 0
        drop(dataa)
        dataa$`Ensembl ID`<-NULL
        dataa$`Gene ID`<-NULL
        data<-as.matrix(dataa)
        numbergene<-nrow(data)
        #list<-c(input$show_vars32,input$show_vars33)
        #target <-c(input$show_vars33)
        #plot_data<-cor(data[,list]) %>% as.data.frame() %>% mutate(var1 = rownames(.)) %>% gather(var2, value, -var1) %>% arrange(desc(value)) %>% group_by(value) %>%filter(row_number()==1)
        #'%ni%' <- Negate('%in%') 
        #filterlist<-filter(plot_data,var1 %in% target && var2 %ni% target)
        #bestone<-filterlist[filterlist$value==max(filterlist$value),1]
        
        #########################################
        nrun <- 100
        my_list <- list()
        alldata<-fread("data/dataall.txt",check.names = FALSE)
        alldata[alldata <=1.5] <- 0
        ldata<-as.matrix(alldata)
        list<-c("gene",input$show_vars32,input$show_vars33)
        fdata<-ldata[,list]
        for (i in 1:nrun) 
        {
          gene<-as.data.frame(sample((fdata[,"gene"]),`numbergene`))
          colnames(gene)<-c("gene")
          rundata<-merge(fdata,gene,by = "gene")
          rundata$gene<-NULL
          rundata$`Ensembl ID`<-NULL
          target <-c(input$show_vars33)
          
          plot_data<-cor(sapply(rundata, function(x) as.numeric(as.character(x))))%>% as.data.frame() %>% mutate(var1 = rownames(.)) %>% gather(var2, value, -var1) %>% arrange(desc(value)) %>% group_by(value) %>%filter(row_number()==1)
          '%ni%' <- Negate('%in%') 
          test<-filter(plot_data,var1 %in% target && var2 %ni% target)
          tisuue<-test[test$value==max(test$value),1]
          my_list[[i]] <- tisuue  
        }
        allout<-as.matrix(table(unlist(my_list)))
        #selectedlist<-allout[target,]
        selectedlist<-allout[rownames(allout) %in% target,] 
        se<-as.matrix(selectedlist)
        sortedm<-se[order(se[,1],decreasing=TRUE),]
        sortedm<-as.matrix(sortedm)
        if(nrow(se)==1){
          
          bt <- function(a) {binom.test(a, 100,p = 0.5, alternative=c("greater"), conf.level = 0.95)$p.value}
          btresult<-mapply(bt,selectedlist)
        }
        else{
          b<-sortedm[2,]
          bt <- function(a,b) {binom.test(a, 100,b, alternative=c("greater"), conf.level = 0.95)$p.value}
          btresult<-mapply(bt,selectedlist,(b/100))
        }
        #itcount<-sum(sapply(my_list, function(x)sum(grepl(bestone$var1, x))))
        #binomresults<-binom.test(`itcount`,100, p = 0.5, alternative = c("greater"), conf.level = 0.95)
        
        #finalout<-cbind(bestone,itcount, binomresults$p.value)
        finalout<-cbind(btresult,selectedlist)
        
        DT::datatable(as.matrix(finalout),class = 'cell-border stripe',colnames=c("Tissue of Analysis","P-Value",
                                                                          
                                                                                  "No. of highest correlations in 100 random iterations"))
                      
        
      }
      else{
        if((input$Adv == TRUE)&& input$pas == TRUE)
        {
          ##############################################
          geneid=strsplit(input$paste,",| |\n")
          geneid<-unlist(geneid)
          geneid<-as.matrix(geneid)
          numbergene<-nrow(geneid)
          ###############################################
          nrun <- 100
          my_list <- list()
          alldata<-fread("data/dataall.txt",check.names = FALSE)
          alldata[alldata <=1.5] <- 0
          ldata<-as.matrix(alldata)
          list<-c("gene",input$show_vars32,input$show_vars33)
          fdata<-ldata[,list]
          for (i in 1:nrun) 
          {
            gene<-as.data.frame(sample((fdata[,"gene"]),`numbergene`))
            colnames(gene)<-c("gene")
            rundata<-merge(fdata,gene,by = "gene")
            rundata$gene<-NULL
            rundata$`Ensembl ID`<-NULL
            target <-c(input$show_vars33)
            
            plot_data<-cor(sapply(rundata, function(x) as.numeric(as.character(x))))%>% as.data.frame() %>% mutate(var1 = rownames(.)) %>% gather(var2, value, -var1) %>% arrange(desc(value)) %>% group_by(value) %>%filter(row_number()==1)
            '%ni%' <- Negate('%in%') 
            test<-filter(plot_data,var1 %in% target && var2 %ni% target)
            tisuue<-test[test$value==max(test$value),1]
            my_list[[i]] <- tisuue 
          }
          
          allout<-as.matrix(table(unlist(my_list)))
          #selectedlist<-allout[target,]
          selectedlist<-allout[rownames(allout) %in% target,] 
          se<-as.matrix(selectedlist)
          sortedm<-se[order(se[,1],decreasing=TRUE),]
          sortedm<-as.matrix(sortedm)
          if(nrow(se)==1){
            
            bt <- function(a) {binom.test(a, 100,p = 0.5, alternative=c("greater"), conf.level = 0.95)$p.value}
            btresult<-mapply(bt,selectedlist)
          }
          else{
            b<-sortedm[2,]
            bt <- function(a,b) {binom.test(a, 100,b, alternative=c("greater"), conf.level = 0.95)$p.value}
            btresult<-mapply(bt,selectedlist,(b/100))
          }
          
          #itcount<-sum(sapply(my_list, function(x)sum(grepl(bestone$var1, x))))
          #binomresults<-binom.test(`itcount`,100 , p = 0.5, alternative = c("greater"), conf.level = 0.95)
          #finalout<-cbind(bestone,itcount, binomresults$p.value)
          finalout<-cbind(btresult,selectedlist)
          DT::datatable(as.matrix(finalout),class = 'cell-border stripe',colnames=c("Tissue of Analysis","P-Value",
                                                                                    "No. of highest correlations in 100 random iterations"))
                        
          
        }
        else{
          if((input$Adv == TRUE)&& !is.null(input$file1))
          {
            geneid <- unique(rv$df[, 1])
            geneid<-as.matrix(geneid)
            numbergene<-nrow(geneid)
            ############################################
            nrun <- 100
            my_list <- list()
            alldata<-fread("data/dataall.txt",check.names = FALSE)
            alldata[alldata <=1.5] <- 0
            ldata<-as.matrix(alldata)
            list<-c("gene",input$show_vars32,input$show_vars33)
            fdata<-ldata[,list]
            for (i in 1:nrun) 
            {
              gene<-as.data.frame(sample((fdata[,"gene"]),`numbergene`))
              colnames(gene)<-c("gene")
              rundata<-merge(fdata,gene,by = "gene")
              rundata$gene<-NULL
              rundata$`Ensembl ID`<-NULL
              target <-c(input$show_vars33)
              
              plot_data<-cor(sapply(rundata, function(x) as.numeric(as.character(x))))%>% as.data.frame() %>% mutate(var1 = rownames(.)) %>% gather(var2, value, -var1) %>% arrange(desc(value)) %>% group_by(value) %>%filter(row_number()==1)
              '%ni%' <- Negate('%in%') 
              test<-filter(plot_data,var1 %in% target && var2 %ni% target)
              tisuue<-test[test$value==max(test$value),1]
              my_list[[i]] <- tisuue 
            }
            
            allout<-as.matrix(table(unlist(my_list)))
            #selectedlist<-allout[target,]
            selectedlist<-allout[rownames(allout) %in% target,] 
            se<-as.matrix(selectedlist)
            sortedm<-se[order(se[,1],decreasing=TRUE),]
            sortedm<-as.matrix(sortedm)
            if(nrow(se)==1){
              
              bt <- function(a) {binom.test(a, 100,p = 0.5, alternative=c("greater"), conf.level = 0.95)$p.value}
              btresult<-mapply(bt,selectedlist)
            }
            else{
              b<-sortedm[2,]
              bt <- function(a,b) {binom.test(a, 100,b, alternative=c("greater"), conf.level = 0.95)$p.value}
              btresult<-mapply(bt,selectedlist,(b/100))
            }
            
            #itcount<-sum(sapply(my_list, function(x)sum(grepl(bestone$var1, x))))
            #binomresults<-binom.test(`itcount`,100 , p = 0.5, alternative = c("greater"), conf.level = 0.95)
            #finalout<-cbind(bestone,itcount, binomresults$p.value)
            finalout<-cbind(btresult,selectedlist)
            DT::datatable(as.matrix(finalout),class = 'cell-border stripe',colnames=c("Tissue of Analysis","P-Value",
                                                                                      "No. of highest correlations in 100 random iterations"))
            
        }}}}}
      })

##############################################################################################################################

#################################### Gene Expression Page Venn Diagram on page 3 #############################################
  
  output$myImagevenn <- renderImage({
    shiny::validate(need(input$show_vars23,"Enter one Tissue of Analysis"))
    shiny::validate(need(input$show_vars22,"Enter one Tissue of Interest"))
    ######################################
    
    if (input$Adv == FALSE&&input$pas == FALSE)
    {
      
      output$selected_Gene_list <- renderText({ 
        paste("PI = Phenotype of Interest:", input$Gene_list)})
      output$selected_show_vars23 <- renderText({ 
        paste(c("TA = Tissue of Analysis:", input$show_vars23),collapse = " ")})
      output$selected_show_vars22 <- renderText({ 
        paste(c("TI = Tissue of Interest:",input$show_vars22),collapse = " ")})
       data <- dataa()
      data<-na_if(data, 0)
      data[data <=1.5] <- NA
      data<-(as.matrix(data))
      list1<-c("Ensembl ID",input$show_vars23)
      list2<-c("Ensembl ID",input$show_vars22)
      act<-data[,list1]
      act<-as.data.frame(act)
      act<-act[rowSums(is.na(act))<(length(act)-1),]
      
      titable<-(data[,list2])
      titable<-as.data.frame(titable)
      titable<-titable[rowSums(is.na(titable))<(length(titable)-1),]
      
      #********************    
      AT<-as.array(act[,"Ensembl ID"])
      TI<-as.array(titable[,"Ensembl ID"])
      PI<-as.array(data[,"Ensembl ID"])
      #****************************************************************
      venn.plot <- reactive({
        overrideTriple=T
        venn.diagram(
          x = list(
            "PI"=PI,
            "TA" = AT,
            "TI" = TI
            
          ),
          main = "",
          filename =outfile, output=TRUE,lty =3,
          na = "remove",imagetype="png",
          col = "transparent",
          fill = c("cornflowerblue", "darkgreen", "red"),
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
          euler.d=TRUE,
          scaled = TRUE,
        )
      })
      outfile <- tempfile(fileext = '.png')
      png(outfile, width = 500, height = 500,type="cairo")
      venn.plot()
      
      dev.off()
      
      #################################################################################
      output$down4 <- downloadHandler(filename = "Gene-venn-diagram.jpeg",contentType = "image/jpeg",content = function(file) {
        ## copy the file from the updated image location to the final download location
        file.copy(outfile, file)})
      #################################################################################
      
      list(src = outfile,
           contentType = 'image/png',
           width = 400,
           height = 400,
           alt = "This is alternate text")
      
    }
    else{    
      if(input$pas == TRUE&&(input$Adv == TRUE))
      {
        output$selected_show_vars23 <- renderText({ 
          paste(c("TA = Tissue of Analysis:", input$show_vars23),collapse = " ")})
        output$selected_show_vars22 <- renderText({ 
          paste(c("TI = Tissue of Interest:",input$show_vars22),collapse = " ")})
        
        data <- dataa()
        data<-na_if(data, 0)
        data[data <=1.5] <- NA
        req(input$paste)
        data<-(as.matrix(data))
        list1<-c("Ensembl ID",input$show_vars23)
        list2<-c("Ensembl ID",input$show_vars22)
        
        geneid=strsplit(input$paste,",| |\n")
        geneid<-unlist(geneid)
        act<-data[data[, 1] %in% geneid,list1]
        act<-as.data.frame(act)
        act<-act[rowSums(is.na(act))<(length(act)-1),]
        titable<-data[data[, 1] %in% geneid,list2]
        titable<-as.data.frame(titable)
        titable<-titable[rowSums(is.na(titable))<(length(titable)-1),]
        #*************************************    
        AT<-as.array(act[,"Ensembl ID"])
        TI<-as.array(titable[,"Ensembl ID"])
        #PI<-as.array(data[,"Ensembl ID"])
        #****************************************************************
        venn.plot <- reactive({venn.diagram(
          x = list(
            "TA" = AT,
            "TI" = TI
            
          ),
          main = "",
          filename =outfile, output=TRUE,lty =3,
          na = "remove",imagetype="png",
          col = "transparent",
          fill = c("cornflowerblue", "pink"),
          alpha = c(0.5,0.5),
          cex = 1.5,
          fontfamily = "serif",
          fontface = "plain",
          cat.col = c("black", "black"),
          cat.cex = 1.5,
          cat.pos = 0,
          cat.dist = 0.07,
          cat.fontfamily = "serif",
          rotation.degree = 270,cat.default.pos = "text",
          scaled = FALSE,
          margin = 0
        )
        })
        outfile <- tempfile(fileext = '.png')
        png(outfile, width = 500, height = 500,type="cairo")
        venn.plot()
        dev.off()
        #################################################################################
        output$down4 <- downloadHandler(filename = "Gene-venn-diagram.jpeg",contentType = "image/jpeg",content = function(file) {
          ## copy the file from the updated image location to the final download location
          file.copy(outfile, file)})
        #################################################################################
        list(src = outfile,
             contentType = 'image/png',
             width = 400,
             height = 400,
             alt = "This is alternate text")
        
      }
      else{
        if((input$Adv == TRUE)&& !is.null(input$file1))
        {
          output$selected_show_vars23 <- renderText({ 
            paste(c("TA = Tissue of Analysis:", input$show_vars23),collapse = " ")})
          output$selected_show_vars22 <- renderText({ 
            paste(c("TI = Tissue of Interest:",input$show_vars22),collapse = " ")})
          
          shiny::validate(need(input$file1,""))
          req(input$file1)
          data <- dataa()
          data<-na_if(data, 0)
          data[data <=1.5] <- NA
          data<-(as.matrix(data))
          list1<-c("Ensembl ID",input$show_vars23)
          list2<-c("Ensembl ID",input$show_vars22)
          ########################################################
          #####################################################
          geneid <- unique(rv$df[, 1])
          act<-data[data[, 1] %in% geneid,list1]
          act<-as.data.frame(act)
          act<-act[rowSums(is.na(act))<(length(act)-1),]
          titable<-data[data[, 1] %in% geneid,list2]
          titable<-as.data.frame(titable)
          titable<-titable[rowSums(is.na(titable))<(length(titable)-1),]
          AT<-as.array(act[,"Ensembl ID"])
          TI<-as.array(titable[,"Ensembl ID"])
          #****************************************************************
          venn.plot <- reactive({venn.diagram(
            x = list(
              "TA" = AT,
              "TI" = TI
              
            ),
            main = "",
            filename =outfile, output=TRUE,lty =3,
            na = "remove",imagetype="png",
            col = "transparent",
            fill = c("cornflowerblue", "pink"),
            alpha = c(0.5,0.5),
            cex = 1.5,
            fontfamily = "serif",
            fontface = "plain",
            cat.col = c("black", "black"),
            cat.cex = 1.5,
            cat.pos = 0,
            cat.dist = 0.07,
            cat.fontfamily = "serif",
            rotation.degree = 270,cat.default.pos = "text",
            scaled = FALSE,
            margin = 0
          )
          })
          outfile <- tempfile(fileext = '.png')
          png(outfile, width = 500, height = 500,type="cairo")
          venn.plot()
          dev.off()
          #################################################################################
          output$down4 <- downloadHandler(filename = "Gene-venn-diagram.jpeg",contentType = "image/jpeg",content = function(file) {
            ## copy the file from the updated image location to the final download location
            file.copy(outfile, file)})
          ################################################################################# 
          list(src = outfile,
               contentType = 'image/png',
               width = 400,
               height = 400,
               alt = "This is alternate text")
          
          
          
        }
        else{
          if((input$Adv == TRUE)&& input$exp == TRUE)
          {
            output$selected_Gene_list <- renderText({ 
              paste("PI = Phenotype of Interest: All genes expressed in", input$show_vars55)})
            output$selected_show_vars23 <- renderText({ 
              paste(c("TA = Tissue of Analysis:", input$show_vars23),collapse = " ")})
            output$selected_show_vars22 <- renderText({ 
              paste(c("TI = Tissue of Interest:",input$show_vars22),collapse = " ")})
            data <- expdata()
            data<-na_if(data, 0)
            data[data <=1.5] <- NA
            data<-(as.matrix(data))
            list1<-c("Ensembl ID",input$show_vars23)
            list2<-c("Ensembl ID",input$show_vars22)
            act<-data[,list1]
            act<-as.data.frame(act)
            act<-act[rowSums(is.na(act))<(length(act)-1),]
            
            titable<-(data[,list2])
            titable<-as.data.frame(titable)
            titable<-titable[rowSums(is.na(titable))<(length(titable)-1),]
            
            #********************    
            AT<-as.array(act[,"Ensembl ID"])
            TI<-as.array(titable[,"Ensembl ID"])
            PI<-as.array(data[,"Ensembl ID"])
            #****************************************************************
            venn.plot <- reactive({
              overrideTriple=T
              venn.diagram(
                x = list(
                  "PI"=PI,
                  "TA" = AT,
                  "TI" = TI
                  
                ),
                
                main = "",
                filename =outfile, output=TRUE,lty =3,
                na = "remove",imagetype="png",
                col = "transparent",
                fill = c("cornflowerblue", "darkgreen", "red"),
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
                euler.d=TRUE,
                scaled = TRUE,
                margin = 0
              )
            })
            outfile <- tempfile(fileext = '.png')
            png(outfile, width = 500, height = 500,type="cairo")
            venn.plot()
            
            dev.off()
            
            #################################################################################
            output$down4 <- downloadHandler(filename = "Gene-venn-diagram.jpeg",contentType = "image/jpeg",content = function(file) {
              ## copy the file from the updated image location to the final download location
              file.copy(outfile, file)})
            #################################################################################
            
            list(src = outfile,
                 contentType = 'image/png',
                 width = 400,
                 height = 400,
                 alt = "This is alternate text")
            
            
          }
          
        }}}},deleteFile = FALSE)
  ###########################################################################################################################################
  output$geneexp <- renderPlot({
    shiny::validate(need(input$show_vars23,""))
    shiny::validate(need(input$show_vars22,""))
    ######################################
    
    if (input$Adv == FALSE&&input$pas == FALSE)
    {
      dataa <- dataa()
      drop(dataa)
      dataa$`Ensembl ID`<-NULL
      dataa$`Gene ID`<-NULL
      data<-as.matrix(dataa)
      data<-na_if(data, 0)
      data[data <=1.5] <- NA
      data<-(as.matrix(data))
      test2<-reshape2::melt(data)
      test3 = na.omit(test2)
      test4<-count(test3,Var2)
      gg<-ggplot(data=test4, aes(x=Var2, y=n,fill =Var2),lab_size = 4) +geom_bar(stat="identity",show.legend = FALSE)+xlab("") + ylab("No. of Expressed Genes")+
        theme(axis.text.x=element_text(size=10, angle=75, vjust=1, hjust=1))
      print(gg)
    }
    else{
      if((input$Adv == TRUE)&& input$exp == TRUE)
      { 
        data <- expdata()
        drop(data)
        data$`Ensembl ID`<-NULL
        data$`Gene ID`<-NULL
        data<-as.matrix(data)
        data<-na_if(data, 0)
        data[data <=1.5] <- NA
        test2<-reshape2::melt(data)
        test3 = na.omit(test2)
        test4<-count(test3,Var2)
        gg<-ggplot(data=test4, aes(x=Var2, y=n,fill =Var2),lab_size = 4) +geom_bar(stat="identity",show.legend = FALSE)+xlab("") + ylab("No. of Expressed Genes")+
          theme_classic()+ theme(axis.text.x=element_text(size=10, angle=67, vjust=1, hjust=1))
        print(gg)
        
      }
      else{
        if(input$pas == TRUE&&(input$Adv == TRUE))
        { 
          data <- dataa()
          geneid=strsplit(input$paste,",| |\n")
          geneid<-unlist(geneid)
          data<-subset(data, `Gene ID` %in% geneid)
          drop(data)
          data$`Ensembl ID`<-NULL
          data$`Gene ID`<-NULL
          data<-as.matrix(data)
          data<-na_if(data, 0)
          data[data <=1.5] <- NA
          test2<-reshape2::melt(data)
          test3 = na.omit(test2)
          test4<-count(test3,Var2)
          gg<-ggplot(data=test4, aes(x=Var2, y=n,fill =Var2),lab_size = 4) +geom_bar(stat="identity",show.legend = FALSE)+xlab("") + ylab("No. of Expressed Genes")+
            theme_classic()+ theme(axis.text.x=element_text(size=10, angle=67, vjust=1, hjust=1))
          print(gg)
        }
        else{
          if((input$Adv == TRUE)&& !is.null(input$file1))
          { 
            data <- dataa()
            geneid <- unique(rv$df[, 1])
            data<-subset(data, `Gene ID` %in% geneid)
            drop(data)
            data$`Ensembl ID`<-NULL
            data$`Gene ID`<-NULL
            data<-as.matrix(data)
            data<-na_if(data, 0)
            data[data <=1.5] <- NA
            test2<-reshape2::melt(data)
            test3 = na.omit(test2)
            test4<-count(test3,Var2)
            gg<-ggplot(data=test4, aes(x=Var2, y=n,fill =Var2),lab_size = 4) +geom_bar(stat="identity",show.legend = FALSE)+xlab("") + ylab("No. of Expressed Genes")+
              theme_classic()+ theme(axis.text.x=element_text(size=10, angle=67, vjust=1, hjust=1))
            print(gg)
          }
        }}}})
  ############################################# Gene TABLE on Gene Expression Page ##########################################################
  output$mytable20 <- DT::renderDataTable({
    shiny::validate(need(input$show_vars23,""))
    shiny::validate(need(input$show_vars22,""))
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
      AT$Type <- ifelse(rowSums(AT[-1:-2] >= 1.5), 'TA', '')
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
        #PI<-as.array(data[,c("Ensembl ID","Gene ID")])
        #*************************************
        AT<-as.data.frame(act)
        AT$Type <- ifelse(rowSums(AT[-1:-2] >= 1.5), 'TA', '')
        TI<-as.data.frame(titable)
        TI$Type <- ifelse(rowSums(TI[-1:-2] >= 1.5), 'TI', '')
        #PI<-cbind(PI,Type='PI')
        tableout<-Reduce(function(x, y) merge(x, y, by = c("Ensembl ID","Gene ID")), list(AT,TI))%>%unite("Type", starts_with("Type"), sep = " ")
        
        ########################
        geneid=strsplit(input$paste,",| |\n")
        geneid<-unlist(geneid)
        table_out<-merge(geneid,tableout,by.x = 1, by.y = "Gene ID")
        colnames(table_out)[1] <- "Gene ID"
        ################################
        
        output$mydownload20=downloadHandler(filename = "Selected_Results.csv",content = function(file){write.csv(table_out,file)})
        DT::datatable((as.matrix(table_out)),class = 'cell-border stripe',
                      caption = "List of Genes with Median gene-level TPM by tissue",options = list(
                        paging = TRUE,
                        pageLength = 5,
                        columnDefs = list(list(className = "dt-center", targets = "_all"))
                      ))
      }
      else{
        if((input$Adv == TRUE)&& !is.null(input$file1))
        {
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
          # PI<-as.array(data[,c("Ensembl ID","Gene ID")])
          #*************************************
          AT<-as.data.frame(act)
          AT$Type <- ifelse(rowSums(AT[-1:-2] >= 1.5), 'TA', '')
          TI<-as.data.frame(titable)
          TI$Type <- ifelse(rowSums(TI[-1:-2] >= 1.5), 'TI', '')
          # PI<-cbind(PI,Type='PI')
          tableout<-Reduce(function(x, y) merge(x, y, by = c("Ensembl ID","Gene ID")), list(AT,TI))%>%unite("Type", starts_with("Type"), sep = " ")
          
          ########################
          geneid <- unique(rv$df[, 1])
          table_out<-merge(geneid,tableout,by.x = 1, by.y = "Gene ID")
          colnames(table_out)[1] <- "Gene ID"
          ################################
          
          output$mydownload20=downloadHandler(filename = "Selected_Results.csv",content = function(file){write.csv(table_out,file)})
          DT::datatable((as.matrix(table_out)),class = 'cell-border stripe',
                        caption = "List of Genes with Median gene-level TPM by tissue",options = list(
                          paging = TRUE,
                          pageLength = 5,
                          columnDefs = list(list(className = "dt-center", targets = "_all"))
                        ))
        }
        else{
          if((input$Adv == TRUE)&& input$exp == TRUE)
          {
            data <- expdata()
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
            AT$Type <- ifelse(rowSums(AT[-1:-2] >= 1.5), 'TA', '')
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
                          ))
          }
          
        }}}
  })
  
  
########################################################################### transcript median
  Tmedian<-reactive({
    if((input$Adv == TRUE)) 
    { fread('Transcript-Median/all.txt',sep = "\t",header = T,check.names = FALSE,na.strings ="", stringsAsFactors= F)
    }
    else{
      if(input$Gene_list == 'Cardiac Arrythmia') 
      { fread('Transcript-Median/Cardiac-Arrythmia.txt',sep = "\t",header = T,check.names = FALSE,na.strings ="", stringsAsFactors= F)
      }
      
      else{
        if (input$Gene_list == 'NDD Genes from SysID')
        {
          fread('Transcript-Median/sysID.txt',sep = "\t",header = T,check.names = FALSE,na.strings ="", stringsAsFactors= F)
        }
        else{
          if (input$Gene_list == 'Obesity')
          {
            fread('Transcript-Median/Obesity.txt',sep = "\t",header = T,check.names = FALSE,na.strings ="", stringsAsFactors= F)
          }
          else{
            if (input$Gene_list == 'Blood Abnormality')
            {
              fread('Transcript-Median/blood-abnormality.txt',sep = "\t",header = T,check.names = FALSE,na.strings ="", stringsAsFactors= F)
            }
            else{
              if (input$Gene_list == 'Breast Abnormality')
              {
                fread('Transcript-Median/Breast_abnormality.txt',sep = "\t",header = T,check.names = FALSE, na.strings ="", stringsAsFactors= F)
              }
              else{
                if (input$Gene_list == 'Abdomen Abnormality')
                {
                  fread('Transcript-Median/abdoman-abnormality.txt',sep = "\t",header = T,check.names = FALSE,na.strings ="", stringsAsFactors= F)
                }
                else{
                  if (input$Gene_list == 'Cardiovascular Abnormality')
                  {
                    fread('Transcript-Median/Cardiovascular-abnormality.txt',sep = "\t",header = T,check.names = FALSE,na.strings ="", stringsAsFactors= F)
                  }
                  else{
                    if (input$Gene_list == 'Connective Tissue Abnormality')
                    {
                      fread('Transcript-Median/Connective_tissue_abnormality.txt',sep = "\t",header = T,check.names = FALSE,na.strings ="", stringsAsFactors= F)
                    }
                    else{
                      if (input$Gene_list == 'Ear Abbnormality')
                      {
                        fread('Transcript-Median/Ear-abbnormality.txt',sep = "\t",header = T,check.names = FALSE,na.strings ="", stringsAsFactors= F)
                      }
                      else{
                        if (input$Gene_list == 'Endocrine Abnormality')
                        {
                          fread('Transcript-Median/Endocrine-abnormality.txt',sep = "\t",header = T,check.names = FALSE,na.strings ="", stringsAsFactors= F)
                        }
                        else{
                          if (input$Gene_list == 'Eye Abnormality')
                          {
                            fread('Transcript-Median/Eye-abnormality.txt',sep = "\t",header = T,check.names = FALSE,na.strings ="", stringsAsFactors= F)
                          }
                          else{
                            if (input$Gene_list == 'Genitourinary Abnormality')
                            {
                              fread('Transcript-Median/Genitourinary_abnormality.txt',sep = "\t",header = T,check.names = FALSE,na.strings ="", stringsAsFactors= F)
                            }
                            else{
                              if (input$Gene_list == 'Growth Abnormality')
                              {
                                fread('Transcript-Median/Growth-abnormality.txt',sep = "\t",header = T,check.names = FALSE,na.strings ="", stringsAsFactors= F)
                              }
                              else{
                                if (input$Gene_list == 'Head Neck Abnormality')
                                {
                                  fread('Transcript-Median/Head-neck-abnormality.txt',sep = "\t",header = T,check.names = FALSE,na.strings ="", stringsAsFactors= F)
                                }
                                else{
                                  if (input$Gene_list == 'Immune Abnormality')
                                  {
                                    fread('Transcript-Median/Immune-abnormality.txt',sep = "\t",header = T,check.names = FALSE,na.strings ="", stringsAsFactors= F)
                                  }
                                  else{
                                    if (input$Gene_list == 'Integument Abbnormality')
                                    {
                                      fread('Transcript-Median/Integument-abbnormality.txt',sep = "\t",header = T,check.names = FALSE,na.strings ="", stringsAsFactors= F)
                                    }
                                    else{
                                      if (input$Gene_list == 'Limbs Abnormality')
                                      {
                                        fread('Transcript-Median/Limbs_abnormality.txt',sep = "\t",header = T,check.names = FALSE,na.strings ="", stringsAsFactors= F)
                                      }
                                      else{
                                        if (input$Gene_list == 'Metabolism Abnormality')
                                        {
                                          fread('Transcript-Median/Metabbolism_abnormality.txt',sep = "\t",header = T,check.names = FALSE,na.strings ="", stringsAsFactors= F)
                                        }
                                        else{
                                          if (input$Gene_list == 'Musculature Abnormality')
                                          {
                                            fread('Transcript-Median/Musculature-abnormality.txt',sep = "\t",header = T,check.names = FALSE,na.strings ="", stringsAsFactors= F)
                                          }
                                          else{
                                            if (input$Gene_list == 'Neoplasm')
                                            {
                                              fread('Transcript-Median/Neoplasm.txt',sep = "\t",header = T,check.names = FALSE,na.strings ="", stringsAsFactors= F)
                                            }
                                            else{
                                              if (input$Gene_list == 'Nervous System Abnormality')
                                              {
                                                fread('Transcript-Median/Nervous-system-abnormality.txt',sep = "\t",header = T,check.names = FALSE,na.strings ="", stringsAsFactors= F)
                                              }
                                              else{
                                                if (input$Gene_list == 'Prenatal Abnormality')
                                                {
                                                  fread('Transcript-Median/Prenatal-abnormality.txt',sep = "\t",header = T,check.names = FALSE,na.strings ="", stringsAsFactors= F)
                                                }
                                                else{
                                                  if (input$Gene_list == 'Respiratory Abnormality')
                                                  {
                                                    fread('Transcript-Median/Respiratory_abnormality.txt',sep = "\t",header = T,check.names = FALSE,na.strings ="", stringsAsFactors= F)
                                                  }
                                                  else{
                                                    if (input$Gene_list == 'Skeletal Abnormality')
                                                    {
                                                      fread('Transcript-Median/Skeletal-abnormality.txt',sep = "\t",header = T,check.names = FALSE,na.strings ="", stringsAsFactors= F)
                                                    }
                                                    else{
                                                      if (input$Gene_list == 'Thoracic Abnormality')
                                                      {
                                                        fread('Transcript-Median/Thoracic-abnormality.txt',sep = "\t",header = T,check.names = FALSE,na.strings ="", stringsAsFactors= F)
                                                      }
                                                      else{
                                                        if (input$Gene_list == 'Voice Abnormality')
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
    shiny::validate(need(input$show_vars15,"Enter one Tissue of Interest"))
    shiny::validate(need(input$show_vars12,"Select a Tissue of Analysis"))
    if (input$Adv == FALSE&&input$pas == FALSE) {
       output$selected_show_vars12 <- renderText({ 
        paste(c("TA = Tissue of Analysis:", input$show_vars12),collapse = " ")})
      output$selected_show_vars15 <- renderText({ 
        paste(c("TI = Tissue of Interest:",input$show_vars15),collapse = " ")})
      
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
          "TA" = AT,
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
        scaled = TRUE,margin=0
      )
      })
      outfile <- tempfile(fileext = '.png')
      png(outfile, width = 500, height = 500,type="cairo")
      venn.plot()
      dev.off()
      #################################################################################
      output$down5 <- downloadHandler(filename = "Transcript-venn-diagram.jpeg",contentType = "image/jpeg",content = function(file) {
        ## copy the file from the updated image location to the final download location
        file.copy(outfile, file)})
      #################################################################################  
      list(src = outfile,
           contentType = 'image/png',
           width = 400,
           height = 400,
           alt = "This is alternate text")
    }
    else
    {
      if(input$pas == TRUE&&(input$Adv == TRUE))
      {
        output$selected_show_vars12 <- renderText({ 
          paste(c("TA = Tissue of Analysis:", input$show_vars12),collapse = " ")})
        output$selected_show_vars15 <- renderText({ 
          paste(c("TI = Tissue of Interest:",input$show_vars15),collapse = " ")})
        
        TMdata <- Tmedian()
        TMdata<-(as.matrix(TMdata))
        TMdata<-trimws(TMdata, "l") #remove space
        list<-c("Gene ID","Ensembl Gene ID","Ensembl Transcript ID",input$show_vars12)
        list2<-c("Gene ID","Ensembl Gene ID","Ensembl Transcript ID",input$show_vars15)
        acti<-TMdata[,list]
        tiss<-TMdata[,list2]
        ACT<-as.data.frame(acti)
        ACT$Type <- ifelse(rowSums(ACT[-1:-3]>=1.5), 'TA', '')
        TOI<-as.data.frame(tiss)
        TOI$Type <- ifelse(rowSums(TOI[-1:-3]>=1.5), 'TI', '')
        trans_table<-merge(ACT,TOI, by = c("Gene ID","Ensembl Gene ID","Ensembl Transcript ID"))%>% tidyr::unite(Type, Type.x, Type.y, sep = ' ', na.rm = TRUE)
        trans_table<-trans_table%>% dplyr::filter(!(Type==" ")) #remove empty colZumns
        ########################
        geneid=strsplit(input$paste,",| |\n")
        geneid<-unlist(geneid)
        gene_id<-merge(geneid,trans_table,by.x = 1, by.y = "Gene ID")
        colnames(gene_id)[1] <- "Gene ID"
        gene_id<-as.matrix(gene_id)
        AT<-cbind(gene_id[grep("TA", gene_id[,"Type"]), "Ensembl Transcript ID"])
        TI<-cbind(gene_id[grep("TI", gene_id[,"Type"]), "Ensembl Transcript ID"])
        ################################
        venn.plot <- reactive({venn.diagram(
          x = list(
            "TA" = AT,
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
          scaled = TRUE,margin=0
        )
        })
        outfile <- tempfile(fileext = '.png')
        png(outfile, width = 500, height = 500,type="cairo")
        venn.plot()
        dev.off()
        #################################################################################
        output$down5 <- downloadHandler(filename = "Transcript-venn-diagram.jpeg",contentType = "image/jpeg",content = function(file) {
          ## copy the file from the updated image location to the final download location
          file.copy(outfile, file)})
        #################################################################################
        list(src = outfile,
             contentType = 'image/png',
             width = 400,
             height = 400,
             alt = "This is alternate text")
      }
      else{
        if((input$Adv == TRUE)&& !is.null(input$file1))
        {
          output$selected_show_vars12 <- renderText({ 
            paste(c("TA = Tissue of Analysis:", input$show_vars12),collapse = " ")})
          output$selected_show_vars15 <- renderText({ 
            paste(c("TI = Tissue of Interest:",input$show_vars15),collapse = " ")})
          
          TMdata <- Tmedian()
          TMdata<-(as.matrix(TMdata))
          TMdata<-trimws(TMdata, "l") #remove space
          list<-c("Gene ID","Ensembl Gene ID","Ensembl Transcript ID",input$show_vars12)
          list2<-c("Gene ID","Ensembl Gene ID","Ensembl Transcript ID",input$show_vars15)
          acti<-TMdata[,list]
          tiss<-TMdata[,list2]
          ACT<-as.data.frame(acti)
          ACT$Type <- ifelse(rowSums(ACT[-1:-3]>=1.5), 'TA', '')
          TOI<-as.data.frame(tiss)
          TOI$Type <- ifelse(rowSums(TOI[-1:-3]>=1.5), 'TI', '')
          trans_table<-merge(ACT,TOI, by = c("Gene ID","Ensembl Gene ID","Ensembl Transcript ID"))%>% tidyr::unite(Type, Type.x, Type.y, sep = ' ', na.rm = TRUE)
          trans_table<-trans_table%>% dplyr::filter(!(Type==" ")) #remove empty colZumns
          ########################
          geneid <- unique(rv$df[, 1])
          gene_id<-merge(geneid,trans_table,by.x = 1, by.y = "Gene ID")
          colnames(gene_id)[1] <- "Gene ID"
          gene_id<-as.matrix(gene_id)
          AT<-cbind(gene_id[grep("TA", gene_id[,"Type"]), "Ensembl Transcript ID"])
          TI<-cbind(gene_id[grep("TI", gene_id[,"Type"]), "Ensembl Transcript ID"])
          venn.plot <- reactive({venn.diagram(
            x = list(
              "TA" = AT,
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
            scaled = TRUE,margin=0
          )
          })
          outfile <- tempfile(fileext = '.png')
          png(outfile, width = 500, height = 500,type="cairo")
          venn.plot()
          dev.off()
          #################################################################################
          output$down5 <- downloadHandler(filename = "Transcript-venn-diagram.jpeg",contentType = "image/jpeg",content = function(file) {
            ## copy the file from the updated image location to the final download location
            file.copy(outfile, file)})
          #################################################################################
          list(src = outfile,
               contentType = 'image/png',
               width = 400,
               height = 400,
               alt = "This is alternate text")
        }
        else{
          if((input$Adv == TRUE)&& input$exp == TRUE)
          {
            output$selected_show_vars12 <- renderText({ 
              paste(c("TA = Tissue of Analysis:", input$show_vars12),collapse = " ")})
            output$selected_show_vars15 <- renderText({ 
              paste(c("TI = Tissue of Interest:",input$show_vars15),collapse = " ")})
            
            TMdata <- Traexpdata()
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
                "TA" = AT,
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
              scaled = TRUE,margin=0
            )
            })
            outfile <- tempfile(fileext = '.png')
            png(outfile, width = 500, height = 500,type="cairo")
            venn.plot()
            dev.off()
            #################################################################################
            output$down5 <- downloadHandler(filename = "Transcript-venn-diagram.jpeg",contentType = "image/jpeg",content = function(file) {
              ## copy the file from the updated image location to the final download location
              file.copy(outfile, file)})
            #################################################################################  
            list(src = outfile,
                 contentType = 'image/png',
                 width = 400,
                 height = 400,
                 alt = "This is alternate text")
          }
        }}}
  }, deleteFile = FALSE)
  ##################################################################################################################################################
  output$tranexp <- renderPlot({
    shiny::validate(need(input$show_vars15,""))
    shiny::validate(need(input$show_vars12,""))
    ######################################
    
    if (input$Adv == FALSE&&input$pas == FALSE)
    {
      TMdata <- Tmedian()
      drop(TMdata)
      TMdata$`Gene ID`<-NULL
      TMdata$`Ensembl Transcript ID`<-NULL
      TMdata$`Ensembl Gene ID`<-NULL
      TMdata<-(as.matrix(TMdata))
      TMdata<-na_if(TMdata, 0)
      TMdata[TMdata <=1.5] <- NA
      test2<-reshape2::melt(TMdata)
      test3 = na.omit(test2)
      test4<-count(test3,Var2)
      gg<-ggplot(data=test4, aes(x=Var2, y=n,fill =Var2),lab_size = 4) +geom_bar(stat="identity",show.legend = FALSE)+xlab("") + ylab("No. of Expressed Transcripts")+
        theme_classic()+ theme(axis.text.x=element_text(size=10, angle=68, vjust=1, hjust=1))
      print(gg)
    }
    else{
      if((input$Adv == TRUE)&& input$exp == TRUE)
      { 
        TMdata <- Traexpdata()
        drop(TMdata)
        TMdata$`Gene ID`<-NULL
        TMdata$`Ensembl Transcript ID`<-NULL
        TMdata$`Ensembl Gene ID`<-NULL
        TMdata<-(as.matrix(TMdata))
        TMdata<-na_if(TMdata, 0)
        TMdata[TMdata <=1.5] <- NA
        test2<-reshape2::melt(TMdata)
        test3 = na.omit(test2)
        test4<-count(test3,Var2)
        gg<-ggplot(data=test4, aes(x=Var2, y=n,fill =Var2),lab_size = 4) +geom_bar(stat="identity",show.legend = FALSE)+xlab("") + ylab("No. of Expressed Transcripts")+
          theme_classic()+ theme(axis.text.x=element_text(size=10, angle=68, vjust=1, hjust=1))
        print(gg)
        
      }
      else{
        if(input$pas == TRUE&&(input$Adv == TRUE))
        { 
          TMdata <- Tmedian()
          geneid=strsplit(input$paste,",| |\n")
          geneid<-unlist(geneid)
          TMdata<-subset(TMdata, `Gene ID` %in% geneid)
          drop(TMdata)
          TMdata$`Gene ID`<-NULL
          TMdata$`Ensembl Transcript ID`<-NULL
          TMdata$`Ensembl Gene ID`<-NULL
          TMdata<-(as.matrix(TMdata))
          TMdata<-na_if(TMdata, 0)
          TMdata[TMdata <=1.5] <- NA
          test2<-reshape2::melt(TMdata)
          test3 = na.omit(test2)
          test4<-count(test3,Var2)
          gg<-ggplot(data=test4, aes(x=Var2, y=n,fill =Var2),lab_size = 4) +geom_bar(stat="identity",show.legend = FALSE)+xlab("") + ylab("No. of Expressed Transcripts")+
            theme_classic()+ theme(axis.text.x=element_text(size=10, angle=68, vjust=1, hjust=1))
          print(gg)
        }
        else{
          if((input$Adv == TRUE)&& !is.null(input$file1))
          { 
            TMdata <- Tmedian()
            geneid <- unique(rv$df[, 1])
            TMdata<-subset(TMdata, `Gene ID` %in% geneid)
            drop(TMdata)
            TMdata$`Gene ID`<-NULL
            TMdata$`Ensembl Transcript ID`<-NULL
            TMdata$`Ensembl Gene ID`<-NULL
            TMdata<-(as.matrix(TMdata))
            TMdata<-na_if(TMdata, 0)
            TMdata[TMdata <=1.5] <- NA
            test2<-reshape2::melt(TMdata)
            test3 = na.omit(test2)
            test4<-count(test3,Var2)
            gg<-ggplot(data=test4, aes(x=Var2, y=n,fill =Var2),lab_size = 4) +geom_bar(stat="identity",show.legend = FALSE)+xlab("") + ylab("No. of Expressed Transcripts")+
              theme_classic()+ theme(axis.text.x=element_text(size=10, angle=68, vjust=1, hjust=1))
            print(gg)
          }
        }}}})
  
  ##################################################################################################################################################
  
  ################################################################ Transcript Table
  output$mytable12 <- DT::renderDataTable({
    shiny::validate(need(input$show_vars15,""))
    shiny::validate(need(input$show_vars12,""))
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
      ACT$Type <- ifelse(rowSums(ACT[-1:-3]>=1.5), 'TA', '')
      TOI<-as.data.frame(tiss)
      TOI$Type <- ifelse(rowSums(TOI[-1:-3]>=1.5), 'TI', '')
      trans_table<-merge(ACT,TOI, by = c("Gene ID","Ensembl Gene ID","Ensembl Transcript ID"))%>% tidyr::unite(Type, Type.x, Type.y, sep = ' ', na.rm = TRUE)
      trans_table<-trans_table%>% dplyr::filter(!(Type==" ")) #remove empty colZumns
      ####################################################################
      
      output$mydownload05=downloadHandler(filename = "Selected_Results.csv",content = function(file){write.csv(trans_table,file)})
      DT::datatable((as.matrix(trans_table)),class = 'cell-border stripe',
                    caption = "List of Transcripts with Median Transcripts-level TPM by tissue",options = list(
                      paging = TRUE,
                      pageLength = 5,
                      columnDefs = list(list(className = "dt-center", targets = "_all"))
                    ))}
    else{
      if(input$pas == TRUE&&(input$Adv == TRUE))
      {
        TMdata <- Tmedian()
        TMdata<-(as.matrix(TMdata))
        TMdata<-trimws(TMdata, "l") #remove space
        list<-c("Gene ID","Ensembl Gene ID","Ensembl Transcript ID",input$show_vars12)
        list2<-c("Gene ID","Ensembl Gene ID","Ensembl Transcript ID",input$show_vars15)
        acti<-TMdata[,list]
        tiss<-TMdata[,list2]
        ACT<-as.data.frame(acti)
        ACT$Type <- ifelse(rowSums(ACT[-1:-3]>=1.5), 'TA', '')
        TOI<-as.data.frame(tiss)
        TOI$Type <- ifelse(rowSums(TOI[-1:-3]>=1.5), 'TI', '')
        trans_table<-merge(ACT,TOI, by = c("Gene ID","Ensembl Gene ID","Ensembl Transcript ID"))%>% tidyr::unite(Type, Type.x, Type.y, sep = ' ', na.rm = TRUE)
        trans_table<-trans_table%>% dplyr::filter(!(Type==" ")) #remove empty colZumns
        ####################################################################
        geneid=strsplit(input$paste,",| |\n")
        geneid<-unlist(geneid)
        gene_id<-merge(geneid,trans_table,by.x = 1, by.y = "Gene ID")
        colnames(gene_id)[1] <- "Gene ID"
        ################################
        
        output$mydownload05=downloadHandler(filename = "Selected_Results.csv",content = function(file){write.csv(gene_id,file)})
        DT::datatable((as.matrix(gene_id)),class = 'cell-border stripe',
                      caption = "List of Transcripts with Median Transcripts-level TPM by tissue",options = list(
                        paging = TRUE,
                        pageLength = 5,
                        columnDefs = list(list(className = "dt-center", targets = "_all"))
                      ))
      }
      else{
        if((input$Adv == TRUE)&& !is.null(input$file1))
        {
          TMdata <- Tmedian()
          TMdata<-(as.matrix(TMdata))
          TMdata<-trimws(TMdata, "l") #remove space
          list<-c("Gene ID","Ensembl Gene ID","Ensembl Transcript ID",input$show_vars12)
          list2<-c("Gene ID","Ensembl Gene ID","Ensembl Transcript ID",input$show_vars15)
          acti<-TMdata[,list]
          tiss<-TMdata[,list2]
          ACT<-as.data.frame(acti)
          ACT$Type <- ifelse(rowSums(ACT[-1:-3]>=1.5), 'TA', '')
          TOI<-as.data.frame(tiss)
          TOI$Type <- ifelse(rowSums(TOI[-1:-3]>=1.5), 'TI', '')
          trans_table<-merge(ACT,TOI, by = c("Gene ID","Ensembl Gene ID","Ensembl Transcript ID"))%>% tidyr::unite(Type, Type.x, Type.y, sep = ' ', na.rm = TRUE)
          trans_table<-trans_table%>% dplyr::filter(!(Type==" ")) #remove empty colZumns
          ##################################################################################################
          geneid <- unique(rv$df[, 1])
          gene_id<-merge(geneid,trans_table,by.x = 1, by.y = "Gene ID")
          colnames(gene_id)[1] <- "Gene ID"
          ################################
          output$mydownload05=downloadHandler(filename = "Selected_Results.csv",content = function(file){write.csv(gene_id,file)})
          DT::datatable((as.matrix(gene_id)),class = 'cell-border stripe',
                        caption = "List of Transcripts with Median Transcripts-level TPM by tissue",options = list(
                          paging = TRUE,
                          pageLength = 5,
                          columnDefs = list(list(className = "dt-center", targets = "_all"))
                        ))
        }
        else{
          if((input$Adv == TRUE)&& input$exp == TRUE)
          {
            TMdata <- Traexpdata()
            TMdata<-(as.matrix(TMdata))
            TMdata<-trimws(TMdata, "l") #remove space
            list<-c("Gene ID","Ensembl Gene ID","Ensembl Transcript ID",input$show_vars12)
            list2<-c("Gene ID","Ensembl Gene ID","Ensembl Transcript ID",input$show_vars15)
            acti<-TMdata[,list]
            tiss<-TMdata[,list2]
            ACT<-as.data.frame(acti)
            ACT$Type <- ifelse(rowSums(ACT[-1:-3]>=1.5), 'TA', '')
            TOI<-as.data.frame(tiss)
            TOI$Type <- ifelse(rowSums(TOI[-1:-3]>=1.5), 'TI', '')
            trans_table<-merge(ACT,TOI, by = c("Gene ID","Ensembl Gene ID","Ensembl Transcript ID"))%>% tidyr::unite(Type, Type.x, Type.y, sep = ' ', na.rm = TRUE)
            trans_table<-trans_table%>% dplyr::filter(!(Type==" ")) #remove empty colZumns
            ####################################################################
            
            output$mydownload05=downloadHandler(filename = "Selected_Results.csv",content = function(file){write.csv(trans_table,file)})
            DT::datatable((as.matrix(trans_table)),class = 'cell-border stripe',
                          caption = "List of Transcripts with Median Transcripts-level TPM by tissue",options = list(
                            paging = TRUE,
                            pageLength = 5,
                            columnDefs = list(list(className = "dt-center", targets = "_all"))
                          ))
          }
        }}
    }})
  
  
  ######################################################Single Gene Analysis Page###########################
  vals6 <- reactiveValues() 
  observeEvent(c(input$Gene_list,input$paste,input$file1,input$exp),
               {
                 
                 if (input$Adv == FALSE) {
                   data <- dataa()
                   drop(data)
                   data<-(data)
                   selectList <- data$`Gene ID`
                   
                   updateSelectizeInput(session = session, inputId = 'mylist', choices = selectList, server = TRUE)}
                 else
                 {
                   if(input$pas == TRUE&&(input$Adv == TRUE))
                   {
                     geneid=strsplit(input$paste,",| |\n")
                     geneid<-unlist(geneid)
                     selectList <- geneid
                     
                     updateSelectizeInput(session = session, inputId = 'mylist', choices = selectList, server = TRUE)
                   }
                   else
                   {
                     if((input$Adv == TRUE)&&!is.null(input$file1))
                     {
                       geneid <- unique(rv$df[, 1])
                       selectList <- geneid
                       
                       updateSelectizeInput(session = session, inputId = 'mylist', choices = selectList, server = TRUE)
                     }
                     else
                     {
                       if((input$Adv == TRUE)&& input$exp == TRUE)
                       {
                         data <- expdata()
                         drop(data)
                         data<-(data)
                         selectList <- data$`Gene ID`
                         updateSelectizeInput(session = session, inputId = 'mylist', choices = selectList, server = TRUE)
                       }
                     }}
                 }
                 
                 
               })
  
  observeEvent(input$Gene_list,
               {
                 
                 output$plot7 <- renderPlot({
                   
                   if (input$Adv == FALSE&&input$pas == FALSE) {
                     shiny::validate(need(input$show_vars4, "Select any one Tissue name"))
                     shiny::validate( need(input$mylist, "Select any one Gene name"))
                     input_genes<-c(input$mylist)
                     geneid_zip<-lapply(input_genes,function(x)paste(x,".zip",sep = "")) ##adding zip extension 
                     geneid_zip_path<-lapply(geneid_zip,function(x)paste("TPM/",x,sep = "")) #Adding path 
                     all_tmp<-vroom(geneid_zip_path)
                    
                     
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
                                      vilolin_data<-merge(list,all_tmp,by=c("Gene_Name","Sample")) 
                                      vilolin_data$Gene_Name<- as.factor(vilolin_data$Gene_Name)
                     
                                    }
                                  })
                     
                     plot<-ggplot(vilolin_data,aes(x=Gene_Name,y=TPM,fill=Sample))
                     Gene_violin<-plot+geom_violin(aes(color = Sample), trim = TRUE,position = position_dodge(0.9) )+xlab("Gene Name") +
                       ylab("TPM")+geom_boxplot(aes(color = Sample), width = 0.15,position = position_dodge(0.9))
                     vals6$gg6 <- Gene_violin
                     print(Gene_violin)
                     
                   }
                   else
                   {
                     if(input$pas == TRUE&&(input$Adv == TRUE))
                     {
                 
                       input_genes<-c(input$mylist)
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
                                        
                                        Gene_Name<-c(input$mylist) ####gene id 
                                        #list<-cbind(Gene_Name,Sample)
                                        list<-CJ(Gene_Name, Sample, unique = TRUE)
                                        vilolin_data<-merge(list,all_tmp,by=c("Gene_Name","Sample")) 
                                        vilolin_data$Gene_Name<- as.factor(vilolin_data$Gene_Name)
                                      }
                                    })
                       plot<-ggplot(vilolin_data,aes(x=Gene_Name,y=TPM,fill=Sample))
                       Gene_violin<-plot+geom_violin(aes(color = Sample), trim = TRUE,position = position_dodge(0.9) )+xlab("Gene Name") +
                         ylab("TPM")+geom_boxplot(aes(color = Sample), width = 0.15,position = position_dodge(0.9))
                       vals6$gg6 <- Gene_violin
                       print(Gene_violin)
                     }
                     else{
                       if((input$Adv == TRUE)&& !is.null(input$file1))
                       {
                         #geneid <- unique(rv$df[, 1])
                         ####################################################################################
                         #data <- dataa()
                         #data<-(as.matrix(data))
                         #all_gtex_gens<-(data[,"Gene ID"])
                         #input_genes<-intersect(all_gtex_gens,geneid)
                         input_genes<-c(input$mylist)
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
                                          
                                          Gene_Name<-c(input$mylist) ####gene id 
                                          #list<-cbind(Gene_Name,Sample)
                                          list<-CJ(Gene_Name, Sample, unique = TRUE)
                                          vilolin_data<-merge(list,all_tmp,by=c("Gene_Name","Sample")) 
                                          vilolin_data$Gene_Name<- as.factor(vilolin_data$Gene_Name)
                                        }
                                      })
                         
                         plot<-ggplot(vilolin_data,aes(x=Gene_Name,y=TPM,fill=Sample))
                         Gene_violin<-plot+geom_violin(aes(color = Sample), trim = TRUE,position = position_dodge(0.9) )+xlab("Gene Name") +
                           ylab("TPM")+geom_boxplot(aes(color = Sample), width = 0.15,position = position_dodge(0.9))
                         vals6$gg6 <- Gene_violin
                         print(Gene_violin)
                         
                       }
                       else{
                         if((input$Adv == TRUE)&& input$exp == TRUE)
                         {
                           #geneid <- unique(rv$df[, 1])
                           ####################################################################################
                           #data <- dataa()
                           #data<-(as.matrix(data))
                           #all_gtex_gens<-(data[,"Gene ID"])
                           #input_genes<-intersect(all_gtex_gens,geneid)
                           input_genes<-c(input$mylist)
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
                                            
                                            Gene_Name<-c(input$mylist) ####gene id 
                                            #list<-cbind(Gene_Name,Sample)
                                            list<-CJ(Gene_Name, Sample, unique = TRUE)
                                            vilolin_data<-merge(list,all_tmp,by=c("Gene_Name","Sample")) 
                                            vilolin_data$Gene_Name<- as.factor(vilolin_data$Gene_Name)
                                          }
                                        })
                           
                           plot<-ggplot(vilolin_data,aes(x=Gene_Name,y=TPM,fill=Sample))
                           Gene_violin<-plot+geom_violin(aes(color = Sample), trim = TRUE,position = position_dodge(0.9) )+xlab("Gene Name") +
                             ylab("TPM")+geom_boxplot(aes(color = Sample), width = 0.15,position = position_dodge(0.9))
                           vals6$gg6 <- Gene_violin
                           print(Gene_violin)}
                       }}}})
               })
  ######################################################## Download Plots 
  output$down6 <- renderUI({
    if(!is.null(vals6$gg6)) {
      downloadButton('OutputFile', '')
    }
  })
  output$OutputFile <- downloadHandler(
    filename =  function() {
      paste("Gene-violin-plot",input$var10,sep=".")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      if(input$var10 == "pdf"){
        pdf(file,width=20, height=15) # open the pdf device
        print(vals6$gg6)
        dev.off()  # turn the device off
      }
      if(input$var10 == "png")
      {png((file),type="cairo",width=800, height=600)
        print(vals6$gg6)
        dev.off()
      }# open the pdf device
    })
  
  ####################################################Single Gene Table #################################################################
  output$table05 <-DT::renderDataTable({
    
    ######################################
    
    if (input$Adv == FALSE&&input$pas == FALSE)
    {
      input_genes<-c(input$mylist)
      shiny::validate(need(input$show_vars4,""))
      geneid_zip<-lapply(input_genes,function(x)paste(x,".zip",sep = "")) ##adding zip extension 
      geneid_zip_path<-lapply(geneid_zip,function(x)paste("TPM/",x,sep = "")) #Adding path 
      all_tmp<-vroom(geneid_zip_path)
      Sample<-c(input$show_vars4)
      Gene_Name<-c(input$mylist)
      list<-CJ(Gene_Name, Sample, unique = TRUE)
      table_data<-merge(list,all_tmp,by=c("Gene_Name","Sample"))
      output$down7 <- renderUI({
        if(!is.null(table_data)) {
          downloadButton('Out_table', '')
        }
      })
      output$Out_table=downloadHandler(filename = "Gene TMP Table.csv",content = function(file){write.csv((table_data),file)})
      datatable(table_data,class = 'cell-border stripe',colnames =c("Gene Name","Tissue","TPM"),
                caption = HTML("Data Source: GTEx Analysis Release V8 <br/>
Displayed numbers represent gene-level TPM by tissue"),rownames= FALSE
      )
    }
    else{    
      if(input$pas == TRUE&&(input$Adv == TRUE))
      {
        #geneid=strsplit(input$paste,",| |\n")
        #geneid<-unlist(geneid)
        ###########################################################################
        #data <- dataa()
        #data<-(as.matrix(data))
        #all_gtex_gens<-(data[,"Gene ID"])
        #input_genes<-intersect(all_gtex_gens,geneid)
        input_genes<-c(input$mylist)
        ##########################################################################
        shiny::validate(need(input$show_vars4,"Select any one Tissue name"))
        geneid_zip<-lapply(input_genes,function(x)paste(x,".zip",sep = "")) ##adding zip extension 
        geneid_zip_path<-lapply(geneid_zip,function(x)paste("TPM/",x,sep = "")) #Adding path 
        all_tmp<-vroom(geneid_zip_path)
        Sample<-c(input$show_vars4)
        Gene_Name<-c(input$mylist)
        list<-CJ(Gene_Name, Sample, unique = TRUE)
        table_data<-merge(list,all_tmp,by=c("Gene_Name","Sample"))
        output$down7 <- renderUI({
          if(!is.null(table_data)) {
            downloadButton('Out_table', '')
          }
        })
        output$Out_table=downloadHandler(filename = "Gene TMP Table.csv",content = function(file){write.csv((table_data),file)})
        datatable(table_data,class = 'cell-border stripe',colnames =c("Gene Name","Tissue","TPM"),
                  caption = HTML("Data Source: GTEx Analysis Release V8 <br/>
Displayed numbers represent gene-level TPM by tissue"),rownames= FALSE
        )
        
      }
      else{
        if((input$Adv == TRUE)&& !is.null(input$file1))
        {
          shiny::validate(need(input$show_vars4,"Select any one Tissue name"))
          #geneid <- unique(rv$df[, 1])
          ############################check whether the inputted genes in Gtex
          #data <- dataa()
          #data<-(as.matrix(data))
          #all_gtex_gens<-(data[,"Gene ID"])
          #input_genes<-intersect(all_gtex_gens,geneid)
          input_genes<-c(input$mylist)
          ##########################################################################
          geneid_zip<-lapply(input_genes,function(x)paste(x,".zip",sep = "")) ##adding zip extension 
          geneid_zip_path<-lapply(geneid_zip,function(x)paste("TPM/",x,sep = "")) #Adding path 
          all_tmp<-vroom(geneid_zip_path)
          Sample<-c(input$show_vars4)
          Gene_Name<-c(input$mylist)
          list<-CJ(Gene_Name, Sample, unique = TRUE)
          table_data<-merge(list,all_tmp,by=c("Gene_Name","Sample"))
          output$down7 <- renderUI({
            if(!is.null(table_data)) {
              downloadButton('Out_table', '')
            }
          })
          output$Out_table=downloadHandler(filename = "Gene TMP Table.csv",content = function(file){write.csv((table_data),file)})
          datatable(table_data,class = 'cell-border stripe',colnames =c("Gene Name","Tissue","TPM"),
                    caption = HTML("Data Source: GTEx Analysis Release V8 <br/>
Displayed numbers represent gene-level TPM by tissue"),rownames= FALSE
          )
          
        }
        else{
          if((input$Adv == TRUE)&& input$exp == TRUE)
          {
            shiny::validate(need(input$show_vars4,"Select any one Tissue name"))
            #geneid <- unique(rv$df[, 1])
            ############################check whether the inputted genes in Gtex
            #data <- dataa()
            #data<-(as.matrix(data))
            #all_gtex_gens<-(data[,"Gene ID"])
            #input_genes<-intersect(all_gtex_gens,geneid)
            input_genes<-c(input$mylist)
            ##########################################################################
            geneid_zip<-lapply(input_genes,function(x)paste(x,".zip",sep = "")) ##adding zip extension 
            geneid_zip_path<-lapply(geneid_zip,function(x)paste("TPM/",x,sep = "")) #Adding path 
            all_tmp<-vroom(geneid_zip_path)
            Sample<-c(input$show_vars4)
            Gene_Name<-c(input$mylist)
            list<-CJ(Gene_Name, Sample, unique = TRUE)
            table_data<-merge(list,all_tmp,by=c("Gene_Name","Sample"))
            output$down7 <- renderUI({
              if(!is.null(table_data)) {
                downloadButton('Out_table', '')
              }
            })
            output$Out_table=downloadHandler(filename = "Gene TMP Table.csv",content = function(file){write.csv((table_data),file)})
            datatable(table_data,class = 'cell-border stripe',colnames =c("Gene Name","Tissue","TPM"),
                      caption = HTML("Data Source: GTEx Analysis Release V8 <br/>
Displayed numbers represent gene-level TPM by tissue"),rownames= FALSE
            )
          }
        }}}
  })
  
  #########################################################################################################################################################
  
  
}

shinyApp(ui, server)

