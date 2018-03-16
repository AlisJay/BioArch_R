#Multi-use funtions####
extract<-function(x,y=input){
  y[[as.character(x)]]
}#used instead of input$x if x = a character string created using paste()

Contains<-function(x,y){
  if(length(x)>1&length(y)>1){
    z<-0
    for(i in 1:length(y)){
      z<-z+sum(x==y[i])
      if(z>0){break}}
    z>0
  }else{sum(x==y)>0}
}#returns true/false if x contains the value y

FreeFix<-function(txt){
  txt<-gsub(",","_",txt)
  txt<-gsub(":",".",txt)
  txt<-gsub("/"," or ",txt)
  txt
}#punctuation fix for free text(textInput),removes commas, colons and slashes

AddTo<-function(x,y){
  if(!(exists(x))){out<-y
  }else{out<-c(get(x),y)}
  out}#if x exists will add y to if not will make new object containing y

IfHassOther<-function(x,y){
  if(Contains(x,"Other")){
    out<-c(x[x!="Other"],FreeFix(y))
  }else{out<-x}
  out}# if x contains "Other" will add y and remove other

IfIsOther<-function(x,y){
  if(x=="Other"){
    out<-FreeFix(y)
  }else{out<-x}
  out}# if x equals "other" will return y else will return x

CustomMeasure<-function(No){
  out<-NULL
  for(i in 1:No){
    out<-tagList(out,
                 textInput(paste0("CMN_",i),paste0("Measurement ",i," Name"),value="NA"),
                 numericInput(paste0("CMV_",i),paste0("Measurement ",i,"(mm)"),value=NA))}
  out}#produces a variable number of custom measurment fields

#Location UI####
Location1<-function(region){
  out<-selectInput("Bone","Bone",c("Please Select a region"=NA))
  if(region=="Skull"){out<-selectInput("Bone","Bone",multiple=TRUE,selected="Frontal",c("Frontal","Parietal","Occipital","Zygomatic","Temporal","Maxilla","Mandible","Palatine","Sphenoid","Nasal","Lacrimal","Ethmoid","Vomer","Inferior nasal cochnea"="Cochnea","Hyoid","Sutural ossicle"="snum_Sutural","All"))}
  if(region=="Vertebrae"){out<-selectInput("Bone","Vertebral region",multiple=TRUE,selected="Atlas",c("Atlas","Axis","Cervical 3-7"="Cervical","Thoracic","Lumbar","Sacrum","supernumerary vertebra"="snum_Vert","All"))}
  if(region=="Pelvis"){out<-selectInput("Bone","Bone",multiple=TRUE,selected="OsCoxa",c("Os coxa"="OsCoxa","Sacrum","Coccyx"))}
  if(region=="Shoulder"){out<-selectInput("Bone","Bone",multiple=TRUE,selected="Scapula",c("Scapula","Clavicle"))}
  if(region=="Leg"){out<-selectInput("Bone","Bone",multiple=TRUE,selected="Femur",c("Femur","Patella","Tibia","Fibula"))}
  if(region=="Foot"){out<-selectInput("Bone","Bone",multiple=TRUE,selected="Talus",c("Talus","Calcaneous","Navicular","Cuboid","Cuniform","Metatarsal","Phalanx","supernumerary foot bones"="snum_Foot","All"))}
  if(region=="Arm"){out<-selectInput("Bone","Bone",multiple=TRUE,selected="Humerus",c("Humerus","Radius","Ulna"))}
  if(region=="Hand"){out<-selectInput("Bone","Bone",multiple=TRUE,selected="Scaphoid",c("Scaphoid","Hamate","Capitate","Lunate","Pisiform","Triquetral","Trapezium","Trapezoid","Metacarpal","Phalanx","supernumerary hand bones"="snum_Hand","All"))}
  if(region=="Thorax"){out<-selectInput("Bone","Bone",multiple=TRUE,selected="R1",c("R1","R2","R3","R4","R5","R6","R7","R8","R9","R10","R11","R12","Sternum","supernumerary rib"="snum_Rib","All"))}
  out
}#creates Bone using region

Location2<-function(Bones){
  out<-tagList(textInput("Location_Free","Overall Location Description",value=NA))
  if(Contains(Bones,"Frontal")){out<-tagList(out,
                                             column(width=3,h4("Frontal"),
                                                    selectInput("Fr_Surface","Surface",multiple=TRUE,c("Endocranial","Ectocranial")),
                                                    selectizeInput("Fr_Features","Features",multiple=TRUE,
                                                                   choice=c("pars orbitalia left"="pars_orbitalia_l","pars orbitalia right"="pars_orbitalia_r","ethmoid notch"="ethmoid_not","frontal sinus left"="sinus_l","frontal sinus right"="sinus_r",
                                                                            "supraorbital margin left"="supraorbital_margin_l","supraorbital margin right"="supraorbital_margin_r","glabella","frontal eminence left"="emin_l","frontal eminence right"="emin_r",
                                                                            "frontal Squama"="frontal_squama","temporal line left"="temporal_line_l","temporal line right"="temporal_line_r","zygomatic process left"="zygomatic_pro_l","zygomatic process right"="zygomatic_pro_r")),
                                                    selectizeInput("Fr_Sutures","Sutures",multiple=TRUE,
                                                                   choice=c("coronal","right frontonasal"="frontonasal_r","left frontonasal"="frontonasal_l","frontosphenoid","right zygomaticofrontal"="zygomaticofrontal_r","left zygomaticofrontal"="zygomaticofrontal_l","right frontomaxillary"="frontomaxillary_r","left frontomaxillary"="frontomaxillary_l",
                                                                            "right frontolacrimal"="frontolacrimal_r","left frontolacrimal"="frontolacrimal_l","frontoethmoidal","metopic","right frontotemporal"="frontotemporal_r","left frontotemporal"="frontotemporal_l"))))}
  if(Contains(Bones,"Parietal")){out<-tagList(out,
                                              column(width=3,h4("Parietal"),
                                                     selectInput("Pr_Side","Side",multiple=TRUE,c("Unknown"="","Right"="_r","Left"="_l")),
                                                     selectInput("Pr_Surface","Surface",multiple=TRUE,c("Endocranial","Ectocranial")),
                                                     selectizeInput("Pr_Feature","Feature",multiple=TRUE,
                                                                    choice=c("parietal striae"="parietal_striae","inferior temporal line"="inf_temporal_line","superior temporal line"="sup_temporal_line","parietal eminence"="emin","sagittal sulcus"="sagittal_sul","sigmoid sulcus"="sigmoid_sul","parietal foramina"="parietal_mina")),
                                                     selectizeInput("Pr_Sutures","Sutures",multiple=TRUE,
                                                                    choice=c("coronal","saggital","lambdoid","squamosal","parietomastoid","sphenoparietal"))))}
  if(Contains(Bones,"Occipital")){
    out<-tagList(out,
                 column(width=3,h4("Occipital"),
                        selectInput("Occipital_Surface","Surface",c("Endocranial","Ectocranial")),
                        selectizeInput("Occipital_Feature","Feature",multiple=TRUE,
                                       choice=c("occipital planum"="occipital_planum","external occipital protuberance"="ex_protub","superior nuchal line"="sup_nuchal_line","inferior nuchal line"="inf__nuchal_line","nuchal planum"="nuchal_planum","external occipital crest"="ex_cr",
                                                "cruciform eminence"="cruciform_emin","transverse sulcus"="transverse_sul","cerebral fossa left"="cerebral_fos_l","cerebral fossa right"="cerebral_fos_r","occipital sulcus"="occipital_sul","cerebellar fossa right"="cerebellar_fos_r","cerebellar fossa left"="cerebellar_fos_l","interal occipital crest"="in_cr",
                                                "occipital condyle right"="con_r","occipital condyle left"="con_l","condylar fossa left"="condylar_fos_l","condylar fossa right"="condylar_fos_r","condylar foramen left"="condylar_for_l","condylar foramen right"="condylar_for_r","hypoglossal canal left"="hypoglossal_can_l","hypoglossal canal right"="hypoglossal_can_r",
                                                "jugular process left"="jugular_pro_l","jugular process right"="jugular_pro_r","jugular notch left"="jugular_not_l","jugular notch right"="jugular_not_l","formen magnum border"="for_magnum_border","basilar part"="basilar")),
                        selectizeInput("Occipital_Sutures","Sutures",multiple=TRUE,
                                       choice=c("lambdoidal","right occipitomastoid"="occipitomastoid_r","left occipitomastoid"="occipitomastoid_l","right petro-occipital"="petrooccipital_r","left petro-occipital","petrooccipital_l","spheno-occipital synchondrosis"="synchondrosis"))))}
  if(Contains(Bones,"Zygomatic")){
    out<-tagList(out,
                 column(width=3,h4("Zygomatic"),
                        selectInput("Zygomatic_Side","Side",c("Unknown"="","Right"="_r","Left"="_l")),
                        selectInput("Zygomatic_Surface","Surface",multiple=TRUE,c("Endocranial","Ectocranial")),
                        selectizeInput("Zygomatic_Feature","Feature",multiple=TRUE,
                                       choice=c("frontal process"="frontal_pro","temporal process"="temporal_pro","maxillary process"="maxillary_pro","zygomaticofacial foramen"="zygomaticofacial_for","masseteric origin"="masseteric","zygomaticotemporal foramen"="zygomaticotemporal_for","orbital surface"="orbit","zygomaticoorbital foramina"="zygomaticoorbital_mina")),
                        selectizeInput("Zygomatic_Sutures","Sutures",multiple=TRUE,choice=c("zygomaticofrontal","zygomaticotemporal","zygomaticomaxillary","sphenozygomatic"))))}
  if(Contains(Bones,"Temporal")){
    out<-tagList(out,
                 column(width=3,h4("Temporal"),
                        selectInput("Temporal_Side","Side",c("Unknown"="","Right"="_r","Left"="_l")),
                        selectInput("Temporal_Surface","Surface",c("Endocranial","Ectocranial")),
                        selectizeInput("Temporal_Feature","Feature ",multiple=TRUE,
                                       choice=c("squama","petrous pyramid"="petrous","external auditory meatus"="ex_auditory_mea","zygomatic process"="zygomatic_p","suprameatal crest"="suprameatal_cr","supramastoid crest"="supramastoid_cr","mastoid process"="mastoid_pro","internal auditory meatus"="in_auditory_mea","sigmoid sulcus"="sigmoid_sul","parietal notch"="parietal_not",
                                                "mastoid notch"="mastoid_not","jugular fossa"="jugular_fos","mandibular fossa"="mandibular_fos","occipital sulcus"="occipital_sul","styloid process"="styloid","articular eminence"="articular_emin","entoglenoid process"="entoglenoid_pro","tympanic part"="tympanic","vaginal process"="vaginal_pro","carotid canal"="carotid_can","stylomastoid foramen"="stylomastoid_for")),
                        selectizeInput("Temporal_Sutures","Sutures",multiple=TRUE,choice=c("squamosal","occipitomastoid","sphenosquamosal","zygomaticotemporal","petro-occipital"="petrooccipital","petrosquamosal","Frontotemporal"))))}
  if(Contains(Bones,"Maxilla")){
    out<-tagList(out,
                 column(width=3,h4("Maxillla"),
                        selectInput("Maxilla_Side","Side",c("Unknown"="","Right"="_r","Left"="_l")),
                        selectInput("Maxilla_Surface","Surface",c("Endocranial","Ectocranial")),
                        selectizeInput("Maxilla_Feature","Feature ",multiple=TRUE,
                                       choice=c("alveolar process"="alveolar_pro","frontal process"="frontal_pro","zygomatic process"="zygomatic_pro","infraorbital foramen"="infraorbital_for","canine fossa"="canine_fos","naso-alveolar clivus"="nasoalveolar_clivus","palatine process"="palatine_pro",
                                                "maxillary sinus"="maxillary_sinus","infraorbital sulcus"="infraorbital_sul","anterior lacrimal crest"="ant_lacrimal_cr","anterior nasal spine"="ant_nasal_sp","canine jugum"="canine_jugum","greater palatine groove"="great_palatine_gr","incisive foramen"="incisive_for")),
                        selectizeInput("Maxilla_Sutures","Sutures",multiple=TRUE,
                                       choice=c("frontomaxillary","zygomaticomaxillary","nasomaxillary","intermaxillary","palatomaxillary","ethmoidomaxillary","lacromaxillary","vomer-maxillary"="vomermaxillary","conchal-maxillary"="conchalmaxillary","sphenomaxillary"))))}
  if(Contains(Bones,"Mandible")){
    out<-tagList(out,
                 column(width=3,h4("Mandible"),
                        selectInput("Mandible_Surface","Surface",multiple=TRUE,c("Internal","External")),
                        selectizeInput("Mandible_Feature","Feature ",multiple=TRUE,
                                       choice=c("corpus","metal foramen right"="mental_for_r","mental formaen left"="mental_for_l","oblique line right"="oblique_line_r","oblique line left"="oblique_line_l","mental protuberance"="mental_protub","masseteric tuberosity right"="masseteric_tub_r","masseteric tuberosity left"="masseteric_tub_l","masseteric fossa right"="masseteric_fos_r","masseteric fossa left"="masseteric_fos_l",
                                                "ramus right"="ramus_r","ramus left"="ramus_l","coronoid process right"="coronoid_pro_r","coronoid process left"="coronoid_pro_l","mandibular notch right"="mandibular_not_r","mandibular notch left"="mandibular_not_l","condylar neck right"="condylar_neck_r","condylar neck left"="condylar_neck_l","mandibular condyle right"="mandibular_con_r","mandibular condyle left"="mandibular_con_l",
                                                "mandibular foramen right"="mandibular_for_r","mandibular foramen left"="mandibular_for_l","lingula right"="lingula_r","lingula left"="lingula_l","mylohoid groove right"="mylohoid_gr_r","mylohoid groove left"="mylohoid_gr_l","pterygoid tuberosities right"="pterygoid_tub_r","pterygoid tuberosies left"="pterygoid_tub_l","mylohyoid line right"="mylohyoid_line_r","mylohyoid line left"="mylohyoid_line_l",
                                                "submandibular fossa right"="submandibular_fos_r","submandibular fossa left"="submandibular_fos_l","mental spines"="mental_sps","digastric fossa"="digastric_fos","sublingual fossa"="sublingual_fos","endocoronoid ridge right"="endocoronoid_rdg_r","endocoronoid ridge left"="endocoronoid_rdg_l","extramolar sulcus right"="extramolar_sul_r","extramolar sulcus left"="extramolar_sul_l"))))}
  if(Contains(Bones,"Palatine")){
    out<-tagList(out,
                 column(width=3,h4("Palatine"),
                        selectInput("Palatine_Side","Side",multiple=TRUE,c("Unknown"="","Right"="_r","Left"="_l")),
                        selectizeInput("Palatine_Feature","Feature ",multiple=TRUE,
                                       choice=c("horizontal plate"="horizontal_plate","greater palatine foramen"="great_palatine_for","pterygopalatine canal"="pterygopalatine_can","posterior nasal spine"="pos_nasal_sp","lesser palatine foramina"="less_palatine_mina","perpendicular plate"="perpendicular_plate")),
                        selectizeInput("Palatine_Sutures","Sutures",multiple=TRUE,choice=c("palatomaxillary","interpalatine","sphenopalatine","vomer-palatine"="vomerpalatine","conchal-palatine"="conchalpalatine","palatoethmoidal"))))}
  if(Contains(Bones,"Sphenoid")){
    out<-tagList(out,
                 column(width=3,h4("Sphenoid"),
                        selectizeInput("Sphenoid_Feature","Feature ",multiple=TRUE,
                                       choice=c("corpus","optic canal right"="optic_can_r","optic canal left"="optic_can_l","sella turcica"="sella_turcica","hypophyseal fossa"="hypophyseal_fos","dorsum sellae","posterior clinoid process"="pos_clinoid_pro","clivus","sphenoidal sinus right"="sinus_r","sphenoidal sinus left"="sinus_l","sphenoidal rostrum"="rostrum","sphenoidal crest"="sphenoidal_cr",
                                                "greater wing right"="great_wing_r","greater wing left"="great_wing_l","superior orbital fissure right"="sup_orbital_fiss_r","superior orbital fissure left"="sup_orbital_fiss_l","foramen rotundum right"="for_rotundum_r","foramen rotundum left"="for_rotundum_l","foramen ovale right"="for_ovale_r","foramen ovale left"="for_ovale_l","foramen spinosum right"="for_spinosum_r","foramen spinosum left"="for_spinosum_l",
                                                "infratemporal crest right"="infratemporal_cr_r","infratemporal crest left"="infratemporal_cr_l","orbital surface right"="orbital_face_r","orbital surface left"="orbital_face_l","lesser wing right"="less_wing_r","lesser_wing_l"="less_wing_r","anterior clinoid process right"="ant_clinoid_pro_r","anterior clinoid process left"="ant_clinoid_pro_l",
                                                "pterygoid process right"="pterygoid_pro_r","pterygoid process left"="pterygoid_pro_l","lateral pterygoid plate right"="lat_pterygoid_plate_r","lateral pterygoid plate left"="lat_pterygoid_plate_l","medial pterygoid plate right"="med_pterygoid_plate_r","medial pterygoid plate left"="med_pterygoid_plate_l","pterygoid fossa right"="pterygoid_fos_right","pterygoid fossa left"="pterygoid_fos_left",
                                                "pterygoid hamulus right"="pterygoid_ham_r","pterygoid hamulus left"="pterygoid_ham_l","pterygoid canal right"="pterygoid_can_r","pterygoid canal left"="pterygoid_can_l")),
                        selectizeInput("Sphenoid_Sutures","Sutures",multiple=TRUE,
                                       choice=c("sphenofrontal","sphenosquamosal right"="sphenosquamosal_r","sphenosquamosal left"="sphenosquamosal_l","sphenozygomatic right"="sphenozygomatic_r","sphenozygomatic left"="sphenozygomatic_l","spheno-occipital synchondrosis"="synchondrosis","sphenoparietal right"="sphenoparietal_r","sphenoparietal left"="sphenoparietal_l",
                                                "sphenopalatine right"="sphenopalatine_r","sphenopalatine left"="sphenopalatine_l","sphenomaxillary right"="sphenomaxillary_r","sphenomaxillary left"="sphenomaxillary_l","sphenovomerian","sphenoethmoidal"))))}
  if(Contains(Bones,"Nasal")){
    out<-tagList(out,
                 column(width=3,h4("Nasal"),
                        selectInput("Nasal_Side","Side",multiple=TRUE,c("Unknown"="","Right"="_r","Left"="_l")),
                        selectInput("Nasal_Surface","Surface",c("Endocranial","Ectocranial")),
                        selectizeInput("Nasal_Sutures","Sutures",multiple=TRUE,choice=c("frontonasal","nasomaxillary","internasal","nasoethmodial"))))}
  if(Contains(Bones,"Lacrimal")){
    out<-tagList(out,
                 column(width=3,h4("Lacrimal"),
                        selectInput("Lacrimal_Side","Side",multiple=TRUE,c("Unknown"="","Right"="_r","Left"="_l")),
                        selectInput("Lacrimal_Surface","Surface",c("Endocranial","Ectocranial")),
                        selectizeInput("Lacrimal_Sutures","Sutures",multiple=TRUE,choice=c("lacrimoconchal","frontolacrimal","ethmoidolacrimal","lacromaxillary"))))}
  if(Contains(Bones,"Ethmoid")){
    out<-tagList(out,
                 column(width=3,h4("Ethmoid"),
                        selectizeInput("Ethmoid_Feature","Feature ",multiple=TRUE,
                                       choice=c("cribiform plate"="cribiform_plate","crista galli"="crista_galli","right labyrinth"="labyrinth_r","left labyrinth"="labyrinth_l")),
                        selectizeInput("Ethmoid_Sutures","Sutures",multiple=TRUE,
                                       choice=c("frontoethmoidal","ethmoidomaxillary right"="ethmoidomaxillary_r","ethmoidomaxillary left"="ethmoidomaxillary_l","ethmoidolacrimal right"="ethmoidolacrimal_r","ethmoidolacrimal left"="ethmoidolacrimal_l","palatoethmoidal right"="palatoethmoidal_r","palatoethmoidal left"="palatoethmoidal_l",
                                                "ethmoidovomerian","ethmoidoconchal right"="ethmoidoconchal_r","ethmoidoconchal left"="ethmoidoconchal_l","nasoethmodial right"="nasoethmodial_r","nasoethmodial left"="nasoethmodial_l","sphenoethmoidal"))))}
  if(Contains(Bones,"Vomer")){
    out<-tagList(out,
                 column(width=3,h4("Vomer"),
                        selectizeInput("Vomer_Sutures","Sutures",multiple=TRUE,
                                       choice=c("sphenovomerian","ethmoidovomerian","vomer-maxillary left"="vomermaxillary_l","vomer-maxillary right"="vomermaxillary_r","vomer-palatine right"="vomerpalatine_r","vomer-palatine left"="vomerpalatine_l"))))}
  if(Contains(Bones,"Cochnea")){
    out<-tagList(out,
                 column(width=3,h4("Cochnea"),
                        selectInput("Cochnea_Side","Side",multiple=TRUE,c("Unknown"="","Right"="_r","Left"="_l")),
                        selectInput("Cochnea_Surface","Surface",multiple=TRUE,c("Endocranial","Ectocranial")),
                        selectizeInput("Cochnea_Sutures","Sutures",multiple=TRUE,choice=c("lacrimoconchal","conchal-maxillary"="conchalmaxillary","conchal-palatine"="conchalpalatine","ethmoidoconchal"))))}
  if(Contains(Bones,"Hyoid")){
    out<-tagList(out,
                 column(width=3,h4("Hyoid"),
                        selectizeInput("Hyoid_Feature","Feature ",multiple=TRUE,choice=c("body","right lesser horn"="less_horn_r","left lesser horn"="less_horn_l","right greater horn"="great_horn_r","left greater horn"="great_horn_l"))))}
  if(Contains(Bones,"Atlas")){
    out<-tagList(out,
                 column(width=3,h4("Atlas"),
                        selectizeInput("Atlas_Feature","Feature",multiple=TRUE,
                                       choice=c("left pedicle"="pedicle_l","right pedicle"="pedicle_r","vertebral arch right side"="arch_r","vertebral arch left side"="arch_l",
                                                "right lamina"="lamina_r","left lamina"="lamina_l","right transverse process"="transverse_pro_r","left transverse process"="transvers_pro_l","right superior articular facet"="sup_artical_fac_r","left superior articular facet"="sup_articular_fac_l",
                                                "right inferior articular facet"="inf_articular_fac_r","left inferior articular facet"="inf_articular_fac_l","right transverse foramen"="transverse_for_r","left transverse foramen"="transverse_for_l","odontoid facet"="odontoid_fac"))))}
  if(Contains(Bones,"Axis")){
    out<-tagList(out,
                 column(width=3,h4("Axis"),
                        selectizeInput("Axis_Feature","Feature",multiple=TRUE,
                                       choice=c("spinous process"="spinous_pro","left pedicle"="pedicle_l","right pedicle"="pedicle_r","vertebral arch right side"="arch_r","vertebral arch left side"="arch_l",
                                                "right lamina"="lamina_r","left lamina"="lamina_l","right transverse process"="transverse_pro_r","left transverse process"="transvers_pro_l","right superior articular facet"="sup_artical_fac_r","left superior articular facet"="sup_articular_fac_l",
                                                "right inferior articular facet"="inf_articular_fac_r","left inferior articular facet"="inf_articular_fac_l","right transverse foramen"="transverse_for_r","left transverse foramen"="transverse_for_l","odontoid process"="odontoid_pro"))))}
  if(Contains(Bones,"Cervical")){
    out<-tagList(out,
                 column(width=3,h4("Cervical Vertebrae"),
                        selectizeInput("Cervical_Bone","Vertebrae",multiple=TRUE,choice=c("C3","C4","C5","C6","C7")),
                        selectizeInput("Cervical_Feature","Feature",multiple=TRUE,
                                       choice=c("body","spinous process"="spinous_pro","left pedicle"="pedicle_l","right pedicle"="pedicle_r","vertebral arch right side"="arch_r","vertebral arch left side"="arch_l",
                                                "right lamina"="lamina_r","left lamina"="lamina_l","right transverse process"="transverse_pro_r","left transverse process"="transvers_pro_l","right superior articular facet"="sup_artical_fac_r","left superior articular facet"="sup_articular_fac_l",
                                                "right inferior articular facet"="inf_articular_fac_r","left inferior articular facet"="inf_articular_fac_l","right transverse foramen"="transverse_for_r","left transverse foramen"="transverse_for_l"))))}
  if(Contains(Bones,"Thoracic")){
    out<-tagList(out,
                 column(width=3,h4("Thoracic Vertebrae"),
                        selectizeInput("Thoracic_Bone","Vertebrae",multiple=TRUE,choice=c("T1","T2","T3","T4","T5","T6","T7","T8","T9","T10","T11","T12")),
                        selectizeInput("Thoracic_Feature","Feature",multiple=TRUE,
                                       choice=c("body","spinous process"="spinous_pro","left pedicle"="pedicle_l","right pedicle"="pedicle_r","vertebral arch right side"="arch_r","vertebral arch left side"="arch_l",
                                                "right lamina"="lamina_r","left lamina"="lamina_l","right transverse process"="transverse_pro_r","left transverse process"="transvers_pro_l","right superior articular facet"="sup_artical_fac_r","left superior articular facet"="sup_articular_fac_l","right inferior articular facet"="inf_articular_fac_r","left inferior articular facet"="inf_articular_fac_l",
                                                "right inferior costal demifacet"="inf_demi_r","left inferior costal demi-facet"="inf_demi_l","right superior costal demi-facet"="sup_demi_r","left superior costal demi-facet"="sup_demi_l","right transverse process costal facet"="transverse_fac_r","left transverse process costal facet"="transverse_fac_l","Right vertebral body facet"="body_fac_r","Left vertebral body facet"="body_fac_l"))))}
  if(Contains(Bones,"Lumbar")){
    out<-tagList(out,
                 column(width=3,h4("Lumbar Vertebrae"),
                        selectizeInput("Lumbar_Bone","Vertebrae",multiple=TRUE,choice=c("L1","L2","L3","L4","L5")),
                        selectizeInput("Lumbar_Feature","Feature",multiple=TRUE,
                                       choice=c("body","spinous process"="spinous_pro","left pedicle"="pedicle_l","right pedicle"="pedicle_r","vertebral arch right side"="arch_r","vertebral arch left side"="arch_l",
                                                "right lamina"="lamina_r","left lamina"="lamina_l","right transverse process"="transverse_pro_r","left transverse process"="transvers_pro_l","right superior articular facet"="sup_artical_fac_r","left superior articular facet"="sup_articular_fac_l",
                                                "right inferior articular facet"="inf_articular_fac_r","left inferior articular facet"="inf_articular_fac_l"))))}
  if(Contains(Bones,c("R3","R4","R5","R6","R7","R8","R9","R10","R11","R12"))){
    B<-Bones[Bones %in% c("R3","R4","R5","R6","R7","R8","R9","R10","R11","R12")]
    for(i in 1:length(B)){
      out<-tagList(out,
                   column(width=3,h4(B[i]),
                          selectInput(paste0(B[i],"_Side"),"Side",c("Unknown"="","Right"="_r","Left"="_l")),
                          selectizeInput(paste0(B[i],"_Feature"),"Feature",multiple=TRUE,choice=c("head","neck","tubercle","angle","shaft","costal groove"="costal_gr","sternal end"="sternal_end","cranial edge"="cranial_edge","caudal edge"="caudal_edge"))))}}
  if(Contains(Bones,"R1")){
    out<-tagList(out,
                 column(width=3,selectInput("R1_Side","Side",multiple=TRUE,c("Unknown"="","Right"="_r","Left"="_l")),
                        selectizeInput("R1_Feature","Feature",multiple=TRUE,
                                       choice=c("head","neck","tubercle","angle","shaft","sternal end"="sternal_end","cranial edge"="cranial_edge","caudal edge"="caudal_edge","subclavian vein groove"="subclavian_gr","brachial plexus groove"="plexus_gr","scalene tubercle"="scalene_tubc"))))}
  if(Contains(Bones,"R2")){
    out<-tagList(out,
                 column(width=3,selectInput("R2_Side","Side",multiple=TRUE,c("Unknown"="","Right"="_r","Left"="_l")),
                        selectizeInput("R2_Feature","Feature",multiple=TRUE,
                                       choice=c("head","neck","tubercle","angle","shaft","costal groove"="costal_gr","sternal end"="sternal_end","cranial edge"="cranial_edge","caudal edge"="caudal_edge","serratus anterior tuberosity"="serratus_tub"))))}
  if(Contains(Bones,"Sternum")){
    out<-tagList(out,
                 column(width=3,h4("Sternum"),
                        selectizeInput("Sternum_Feature","Feature",multiple=TRUE,
                                       choice=c("manubrium","right clavicular notch"="clavicular_not_r","left clavicular notch"="clavicular_not_l","jugular notch"="jugular_not","corpus sterni"="corpus","xiphoid process"="xiphoid_pro","1st right costal notch"="costal1_not_r","1st left costal notch"="costal1_not_l","2nd right costal notch"="costal2_not_r","2nd left costal notch"="costal2_not_l","3rd right costal notch"="costal3_not_r","3rd left costal notch"="costal3_not_l",
                                                "4th right costal notch"="costal4_not_r","4th left costal notch"="costal4_not_l","5th right costal notch"="costal5_not_r","5th left costal notch"="costal5_not_l","6th right costal notch"="costal6_not_r","6th left costal notch"="costal6_not_l","7th right costal notch"="costal7_not_r","7th left costal notch"="costal7_not_l"))))}
  if(Contains(Bones,"Clavicle")){
    out<-tagList(out,
                 column(width=3,h4("Clavicle"),
                        selectInput("Clavicle_Side","Side",multiple=TRUE,c("Unknown"="","Right"="_r","Left"="_l")),
                        selectizeInput("Clavicle_Feature","Feature",multiple=TRUE,
                                       choice=c("costal tuberosity"="costal_tub","subclavian sulcus"="subclavian_sul","conoid tubercle"="conoid_tubc","trapezoid line"="trapezoid_line","superior surface"="sup_face","trapezius attachment"="trapezius_att","deltoideus attachment"="deltoideus_att","pectoralis major attachment"="pectoralis_att"))))}
  if(Contains(Bones,"Scapula")){
    out<-tagList(out,
                 column(width=3,h4("Scapula"),
                        selectInput("Scapula_Side","Side",multiple=TRUE,c("Unknown"="","Right"="_r","Left"="_l")),
                        selectizeInput("Scapula_Feature","Feature",multiple=TRUE,
                                       choice=c("coracoid process"="coracoid_pro","acromion process"="acromion_pro","glenoid fossa"="glenoid_fos","superior border"="sup_border","Scapula notch/foramen"="scapula_not","superior angle"="sup_angle","medial border"="med_border","inferior angle"="inf_angle","lateral border"="lat_border",
                                                "subscapular fossa"="subscapular_fos","oblque ridges"="oblique_rdg","scapular spine"="scapular_sp","supraspinous fossa"="supraspinous_fos","infraspinous fossa"="infraspinous_fos","scapula neck"="neck","supraglenoid tubercle"="supraglenoid_tubc","infraglenoid tubercle"="infraglenoid_tubc"))))}
  if(Contains(Bones,"Humerus")){
    out<-tagList(out,
                 column(width=3,h4("Humerus"),
                        selectInput("Humerus_Side","Side",multiple=TRUE,c("Unknown"="","Right"="_r","Left"="_l")),
                        selectizeInput("Humerus_Feature","Feature",multiple=TRUE,
                                       choice=c("head","anatomical neck"="anatomical_neck","surgical neck"="surgical_neck","greater tubercle"="great_tubc","lesser tubercle"="less_tubc","intertubercular sulcus"="intertubercular_sul","greater tubercle crest"="great_tubc_cr","lesser tubercle crest"="less_tubc_cr",
                                                "trochlea","capitulum","coronoid fossa"="coronoid_fos","radial fossa"="radial_fos","medial supracondylar crest"="med_supracondylar_cr","lateral supracondylar crest"="lat_supracondylar_cr","medial epicondyle"="med_epicondyle","lateral epicondyle"="lat_epicondyle","olecranon fossa"="olecranon_fos",
                                                "shaft","nutrient foramen"="nutrient_for","deltoid tuberosity"="deltoid_tub","radial sulcus"="radial_sul"))))}
  if(Contains(Bones,"Radius")){
    out<-tagList(out,h4("Radius"),
                 column(width=3,selectInput("Radius_Side","Side",c("Unknown"="","Right"="_r","Left"="_l")),
                        selectizeInput("Radius_Feature","Feature",multiple=TRUE,
                                       choice=c("head","neck","radial tuberosity"="radial_tub","shaft","nutrient foramen"="nutrient_for","interosseous crest"="interosseous_cr","oblique line"="oblique_line","pronator teres insersion"="pronator_att","ulnar notch"="ulna_not","distal articular surface"="dis_articular_face","styloid process"="styloid_pro","dorsal tubercle"="dorsal_tubc"))))}
  if(Contains(Bones,"Ulna")){
    out<-tagList(out,h4("Ulna"),
                 column(width=3,selectInput("Ulna_Side","Side",c("Unknown"="","Right"="_r","Left"="_l")),
                        selectizeInput("Ulna_Feature","Feature",multiple=TRUE,
                                       choice=c("olecranon process"="olecranon_pro","trochlear notch"="trochlear_not","guiding ridge"="guiding_rdg","coronoid process"="coronoid_pro","ulnar tuberosity"="ulna_tub","radial notch"="radial_not",
                                                "shaft","nutrient foramen"="nutrient_for","interosseous crest"="interosseous_cr","pronator ridge"="pronator_rdg","styloid process"="styloid_pro","extensor carpi ulnaris groove"="ulnaris_gr","radial articulation"="radial_articulation"))))}
  if(Contains(Bones,"Phalanx")){
    out<-tagList(out,
                 column(width=3,h4("Phalanxs"),
                        selectInput("Phalanx_Side","Side",c("Unknown"="","Right"="_r","Left"="_l")),
                        selectInput("Phalanx_number","Phalanx",c("proximal 1"="_p1","distal 1"="_d1","proximal 2"="_p2","intermediate 2"="_i2","distal 2"="_d2","proximal 3"="_p3","intermediate 3"="_i3","distal3"="_d3","proximal 4"="_p4","intermediate 4"="_i4","distal 4"="_d4","proximal 5"="_p5","intermediate 5"="_i5","distal 5"="_d5")),
                        selectizeInput("Phalanx_Feature","Feature",multiple=TRUE,choice=c("head","shaft","base"))))}
  if(Contains(Bones,c("Metacarpal","Metatarsal"))){
    M<-Bones[Bones %in% c("Metacarpal","Metatarsal")]
    for(i in 1:length(M)){
      out<-tagList(out,h4(M[i]),
                   selectInput(paste0(M[i],"_side"),"Side",c("Unknown"="","Right"="_r","Left"="_l")),
                   selectInput(paste0(M[i],"_number"),"Ray",c("1st"="_1","2nd"="_2","3rd"="_3","4th"="_4","5th"="_5")),
                   selectizeInput(paste0(M[i],"_feature"),"Feature",multiple=TRUE,choice=c("head","shaft","base")))}}
  if(Contains(Bones,c("Scaphoid","Hamate","Capitate","Lunate","Pisiform","Triquetral","Trapezium","Trapezoid","Cuboid","Navicular"))){
    H<-Bones[Bones %in% c("Scaphoid","Hamate","Capitate","Lunate","Pisiform","Triquetral","Trapezium","Trapezoid","Cuboid","Navicular")]
    for(i in 1:length(H)){out<-selectInput(out,h4(H[i]),paste0(H[i],"_side"),"Side",c("Unknown"="","Right"="_r","Left"="_l"))}
    }
  if(Contains(Bones,"Sacrum")){
    out<-tagList(out,
                 column(width=3,h4("Sacrum"),
                        selectizeInput("Sacrum_Feature","Feature",multiple=TRUE,
                                       choice=c("promontory","right alae"="alae_r","left alae"="alae_l","right auricular surface"="auricular_face_r","left auricular surface"="auricular_face_l","right superior articular facte"="sup_articular_fac_r","left superior articular facte"="sup_articular_fac_l","dorsal wall"="dorsal_wall","median crest"="median_cr","right anterior sacral foramina"="ant_sacral_mina_r","left anterior sacral foramina"="ant_sacral_mina_l","transverse lines"="transverse_line"))))}
  if(Contains(Bones,"OsCoxa")){
    out<-tagList(out,
                 column(width=3,h4("OsCoxa"),
                        selectInput("OsCoxa_Side","Side",c("Unknown"="","Right"="_r","Left"="_l")),
                        selectizeInput("OsCoxa_Feature","Feature",multiple=TRUE,
                                       choice=c("illium","ischium","pubis","acetabulum","acetabular fossa"="acetabular_fos","lunate surface"="lunate_face","iliac pilar"="illiac_pilar","iliac tubercle"="iliac_tubc","iliac crest"="iliac_cr","anterior gluteal line"="ant_gluteal_line","posterior gluteal line"="pos_gluteal_line","inferior gluteal line"="inf_gluteal_line","iliac fossa"="iliac_fos","arcuate line"="arcuate_line",
                                                "posterior inferior illiac spine"="pos_inf_illiac_sp","posterior superior illiac spine"="pos_sup_illiac_sp","anterior inferior illiac spine"="ant_inf_illiac_sp","anterior superior illiac spine"="ant_sup_illiac_sp","iliac tuberosity"="iliac_tub","auricular surface"="auricular_face","preauricular sulcus"="preauricular_sul","greater sciatic notch"="great_sciatic_not","lesser sciatic notch"="less_Sciatic_not",
                                                "ishial tuberocity"="ishial_tub","ishial spine"="ishial_sp","ishiopubic ramus"="ishiopubic_ram","iliopubic ramus"="iliopubic_ram","iliopubic eminence"="iliopubic_emin","pubic symphysis"="pubic_symphysis","obturator groove"="obturator_gr","obturator foramen"="obturator_for"))))}
  if(Contains(Bones,"Femur")){
    out<-tagList(out,
                 column(width=4,h4("Femur"),
                        selectInput("Femur_Side","Side",c("Unknown"="","Right"="_r","Left"="_l")),
                        selectizeInput("Femur_Feature","Feature",multiple=TRUE,
                                       choice=c("head","fovae capitis"="fovae_capitis","neck","greater trochanter"="great_trochanter","lesser trochanter"="less_trochanter","intertrochanteric line"="intertrochanteric_line","intertrochanteric crest"="intertrochanteric_cr","trochanteric fossa"="trochanteric_fos","obturator externus groove"="obtutator_externus_gr","gluteal tuberosity/line"="gluteal_tub","pectineal line"="pectineal_line","linea aspera"="linea_aspera",
                                                "shaft","nutrient foramen"="nutrient_for","spiral line"="spiral_line","medial supracondylar line"="med_supracondylar_line","lateral supracondylar line"="lat_supracondylar_line","popiteal surface"="popiteal_face","lateral condyle"="lat_con","medial condyle"="med_con","popliteal groove"="popliteal_gr","lateral epicondyle"="lat_epicondyle","medial epicondyle"="med_epicondyle","adductor tubercle"="adductor_tubc",
                                                "intercondylar fossa/notch"="intercondylar_fos","patellar surface"="patellar_face"))))}
  if(Contains(Bones,"Patella")){
    out<-tagList(out,
                 column(width=3,h4("Patella"),
                        selectInput("Patella_Side","Side",c("Unknown"="","Right"="_r","Left"="_l")),
                        selectizeInput("Patella_Feature","Feature",multiple=TRUE,choice=c("apex","lateral articular facet"="lat_articular_fac","medial articular facet"="med_articular_fac","anterior surface"="ant_face"))))}
  if(Contains(Bones,"Tibia")){
    out<-tagList(out,
                 column(width=3,h4("Tibia"),
                        selectInput("Tibia_Side","Side",c("Unknown"="","Right"="_r","Left"="_l")),
                        selectizeInput("Tibia_Feature","Feature",multiple=TRUE,
                                       choice=c("tibial plateau"="plateau","medial condyle"="med_con","lateral condyle"="lat_con","intercondylar eminenece"="intercondylar_emin","medial intercondylar tubercle"="med_intercondylar_tubc","lateral intercondylar tubercle"="lat_intercondylar_tubc","superior fibular articular facet"="sup_fibular_fac",
                                                "tibial tuberocity"="tibial_tub","shaft","popliteal/soleal line"="popliteal_line","nutrient foramen"="nutrient_for","anterior surface"="ant_face","medial surface"="med_face","interosseous crest"="interosseous_cr","interosseous surface"="interosseous_face",
                                                "medial malleous"="med_malleous","fibular notch"="fibular_not","inferior tibial articular facet"="inf_tibial_fac","malleolar groove"="malleolar_gr","distal articular surface"="dist_articular_face"))))}
  if(Contains(Bones,"Fibula")){
    out<-tagList(out,
                 column(width=3,h4("Fibula"),
                        selectInput("Fibula_Side","Side",c("Unknown"="","Right"="_r","Left"="_l")),
                        selectizeInput("Fibula_Feature","Feature",multiple=TRUE,
                                                           choice=c("head","styloid process"="styloid_pro","proximal articular surface"="prox_articular_face","shaft","interosseous crest"="interosseous_cr","nutrient foramen"="nutrient_for","lateral malleolus"="lat_malleolus","distal articular surface"="dist_articular_face","malleolar fossa"="malleolar_fos","peroneal groove"="peroneal_gr"))))}
  if(Contains(Bones,"Talus")){
    out<-tagList(out,
                 column(width=3,h4("Talus"),
                        selectInput("Talus_Side","Side",c("Unknown"="","Right"="_r","Left"="_l")),
                        selectizeInput("Talus_Feature","Feature",multiple=TRUE,
                                       choice=c("head","body","trochlea","neck","flexor hallucius longus groove"="flexor_hallucius_longus_gr","anterior calcaneal(subtalar) articular surface"="ant_calcaneal_face","middle calcaneal(subtalar) articular surface"="mid_calcaneal_face","posterior calcaneal(subtalar) articular surface"="pos_calcaneal_face","sulcus tali"="tali_sul"))))}
  if(Contains(Bones,"Calcaneous")){
    out<-tagList(out,
                 column(width=3,h4("Calcaneous"),
                        selectInput("Calcaneous_Side","Side",c("Unknown"="","Right"="_r","Left"="_l")),
                        selectizeInput("Calcaneous_Feature","Feature",multiple=TRUE,
                                       choice=c("calcaneal tuber"="tuber","lateral process"="lat_pro","medial process"="med_pro","sustentaculum tali"="sustentaculum_tali","sustentacular sulcus"="sustentacular_sul","peroneal tubercle"="peroneal_tubc"))))}
  if(Contains(Bones,"Cuniform")){
    out<-tagList(out,
                 column(width=3,h4("Cuniform"),
                        selectInput("Cuniform_Side","Side",c("Unknown"="","Right"="_r","Left"="_l")),
                        selectInput("Cuniform_number","Number",choice=c("medial(1st)"="_med","intermediate(2nd)"="_inter","lateral(3rd)"="_lat"))))}
  if(Contains(Bones,"snum_Vert")){
    out<-tagList(out,
                 column(width=3,h4("Supernumerary vertebrae"),
                        selectInput("snum_Vert_Type","Type",c("Cervical","Thorasic","Lumbar","Sacral","cervical/thorasic borderline"="Cervical_Thorasic","thorasic/lumbar borderline"="Thorasic_Lumbar","lumbar/sacral borderline"="Lumbar_Sacral","unknown")),
                        selectizeInput("snum_Vert_Feature","Feature",multiple=TRUE,
                                       choice=c("body","spinous process"="spinous_pro","left pedicle"="pedicle_l","right pedicle"="pedicle_r","vertebral arch right side"="arch_r","vertebral arch left side"="arch_l",
                                                "right lamina"="lamina_r","left lamina"="lamina_l","right transverse process"="transverse_pro_r","left transverse process"="transvers_pro_l","right superior articular facet"="sup_artical_fac_r","left superior articular facet"="sup_articular_fac_l",
                                                "right inferior articular facet"="inf_articular_fac_r","left inferior articular facet"="inf_articular_fac_l","right transverse foramen"="transverse_for_r","left transverse foramen"="transverse_for_l",
                                                "right inferior costal demifacet"="inf_demi_r","left inferior costal demi-facet"="inf_demi_l","right superior costal demi-facet"="sup_demi_r","left superior costal demi-facet"="sup_demi_l","right transverse process costal facet"="transverse_fac_r","left transverse process costal facet"="transverse_fac_l","Right vertebral body facet"="body_fac_r","Left vertebral body facet"="body_fac_l"))))}
  if(Contains(Bones,"snum_Rib")){
    out<-tagList(out,
                 column(width=3,h4("Supernumerary Rib"),
                        selectInput("snum_Rib_Type","Type",c("Cervical","Intrathoracic","Lumbar","Sacral","other/unknown")),
                        selectInput("snum_Rib_Side","Side",c("Unknown"="","Right"="_r","Left"="_l")),
                        selectizeInput("snum_Rib_Feature","Feature",multiple=TRUE,
                                       choice=c("head","neck","tubercle","angle","shaft","costal groove"="costal_gr","sternal end"="sternal_end","cranial edge"="cranial_edge","caudal edge"="caudal_edge"))))}
  if(Contains(Bones,"snum_Foot")){
    out<-tagList(out,
                 column(width=3,h4("Supernumery Foot bones"),
                        selectInput("snum_Foot_Side","Side",c("Unknown"="","Right"="_r","Left"="_l")),
                        selectInput("snum_Foot_Type","Type",multiple=TRUE,c("other/unknown","os tibiale externum","os trigonum","os peroneum","os intermetatarseum","os subfibulare","os supranaviculare","os subtibiale","os supratalare","os calcaneus secundarius","os vesalianium","os intercuneiforme","os cuboideum secundarium","os tallus accesorius","os tallus secundarius","metatarsophalangeal sesamoid","interphalangeal sesmoid"))))}
  if(Contains(Bones,"snum_Hand")){
    out<-tagList(out,
                 column(width=3,h4("Supernumery Hand bones"),
                        selectInput("snum_Hand_Side","Side",c("Unknown"="","Right"="_r","Left"="_l")),
                        selectInput("snum_Hand_Type","Type",multiple=TRUE,c("other/unknown","os centrale","os vesalianum carpi","os gruberi","os radiale externum","os epitrapezium","os epilunatum","os radiostyloideum","os hypolunatum","os hypotriquetrum","os epitriquestum","os triangulare","metacarpalphalangeal sesmoid","interphalangeal sesmoid",
                                                                  "pisiforme secundarium","os hamuli proprium","os hamulare basale","os ulnare externum","os capitatum secundarium","os subcapitatum","os subcapitatum","os styloideum","os parastyloideum","os metastyloideum","os trapezium secundarium","os praetrapezium","os paratrapezium","os trapezoideum secundarium"))))}
  if(Contains(Bones,"snum_Sutural")){
    out<-tagList(h5("nb this section is for the recording of abnormalities on/relating to the supernummery bone, the presence of the bone should be recorded in the skull tab.Descriptions of the types of ossicle can also be found in the skull tab"),
                                                  selectInput(paste0("side_",Add),"Side",c("Midline/Unknown"="","Right"="_r","Left"="_l")),
                                                  selectInput(paste0("bone2_",Add),"Type",c("Inca","Lambda","Lambdoidal","Pterion","Bregma","Coronal","Saggittal")))}
  out
}#produces location detail UI for each recorded bone

#Trauma UI####
Fracture<-function(Type,region){
  if(Contains(Type,"Fracture")){
    if(region=="Skull"){out<-selectInput("fract_Specific","Named Fracture",c("Custom","Lefort1","Lefort2","Lefort3"))}
    if(region=="Shoulder"){out<-selectInput("fract_Specific","Named Fracture",c("Custom"))}
    if(region=="Pelvis"){out<-selectInput("fract_Specific","Named Fracture",c("Custom"))}
    if(region=="Arm"){out<-selectInput("fract_Specific","Named Fracture",c("Custom","Colles","Parry"))}
    if(region=="Hand"){out<-selectInput("fract_Specific","Named Fracture",c("Custom"))}
    if(region=="Leg"){out<-selectInput("fract_Specific","Named Fracture",c("Custom"))}
    if(region=="Foot"){out<-selectInput("fract_Specific","Named Fracture",c("Custom"))}
    if(region=="Thorax"){out<-selectInput("fract_Specific","Named Fracture",c("Custom"))}
    if(region=="Vertebrae"){out<-selectInput("fract_Specific","Named Fracture",c("Custom"))}
  }else{out<-h5(br())}
  out
}#produces fract_Specific using region

TraumaUI<-function(input){
  
  out<-tagList(fixedPage(column(width=4,textInput("Trauma_Free","Overall Description",value=NA),
                                selectInput("Trauma_Heal","Healing state",c("No Healing,Postmortem"="Post","No Healing,Perimortem"="Peri","Antemortem, Healing"="Anti_Healing","Antemortem, Healed"="Ante_Healed","Unknown")),
                                textInput("Trauma_Heal2","Reason for healing state determination",value=NA)),
                         column(width=4,h4("Measurements"),h5("n.b You should include enough measurments to give a reliable impression of the abnormality.The RM Columns give recomended Measurements for each trauma type"),
                                numericInput("CustomMeasure","Number of measurments",value=NA),
                                actionButton("AddCM","Add fields")),
                         column(width=4,uiOutput("CM"))))
  for(i in 1:length(input$Trauma_Type)){
    if(input$Trauma_Type[i]=="Fracture" && input$fract_Specific!="Custom"){
        source("Options/Fractures.R",local=TRUE)
        FractUI<-paste0(input$fract_Specific,"_UI")
        out<-tagList(out,fixedPage(get(FractUI)))
    }else{
      source("Options/Trauma.R",local=TRUE)
      UI<-paste0(input$Trauma_Type[i],"_UI")
      out<-tagList(out,fixedPage(get(UI)))}}
  out<-tagList(out,h1(br()),h1(br()),h1(br()),h1(br()))
  out
}# produces trauma ui based on Trauma_Type, and fract_Specific if relevant

#TaphonmicUI####
TaphUI<-function(input){
  source("Options/Taph.R",local=TRUE)
  out<-tagList(fixedPage(column(width=4,textInput("Taph_Free","Overall Description",value=NA),
                                selectInput("Taph_Heal","Healing state",c("No Healing,Postmortem"="Post")),
                                selectInput("Taph_Heal2","Reason for healing state determination",c("Taphonomic Damage"))),
                         column(width=4,h4("Measurements"),h5("n.b You should include enough measurments to give a reliable impression of the abnormality"),
                                numericInput("CustomMeasure","Number of measurments",value=NA),
                                actionButton("AddCM","Add fields")),
                         column(width=4,uiOutput("CM"))))
  for(i in 1:length(input$Taph_Type)){
    UI<-paste0(input$Taph_Type[i],"_UI")
    out<-tagList(out,fixedPage(get(UI)))}
  out<-tagList(out,h1(br()),h1(br()),h1(br()),h1(br()))
  out
}#produces taphonomic ui based on Taph_Type
#Pathology UI####
Path1<-function(Type,region){
  if(length(Type)>1){
    if(region=="Skull"){out<-selectInput("Path_Specific","Known Complex Abnormality",c("Custom"="SK_CC"))}
    if(region=="Vertebrae"){out<-selectInput("Path_Specific","Known Complex Abnormality",c("Custom"="V_CC"))}
    if(region=="Pelvis"){out<-selectInput("Path_Specific","Known Complex Abnormality",c("Custom"="P_CC"))}
    if(region=="Shoulder"){out<-selectInput("Path_Specific","Known Complex Abnormality",c("Custom"="SH_CC"))}
    if(region=="Leg"){out<-selectInput("Path_Specific","Known Complex Abnormality",c("Custom"="L_CC"))}
    if(region=="Foot"){out<-selectInput("Path_Specific","Known Complex Abnormality",c("Custom"="F_CC","Tufting and resorption of distal phalanges"="F_C02"))}
    if(region=="Arm"){out<-selectInput("Path_Specific","Known Complex Abnormality",c("Custom"="A_CC"))}
    if(region=="Hand"){out<-selectInput("Path_Specific","Known Complex Abnormality",c("Custom"="H_CC","Tufting and resorption of distal phalanges"="H_C01"))}
    if(region=="Thorax"){out<-selectInput("Path_Specific","Known Complex Abnormality",c("Custom"="T_CC"))}
  }else{
    if(Type=="Loss"){
      if(region=="Skull"){out<-selectInput("Path_Specific","Know Loss Abnormality",c("Custom"="SK_CL","Ectocranial porosis: of the Parietal/Occipital or Frontal Squama/ Zygomatic"="SK_L01a","Orbital Ectocranial Porosis"="SK_L01b", "Supraorbital frontal groove(s)"="SK_L02", "Enlarged parietal foramina"="SK_L09"))}
      if(region=="Vertebrae"){out<-selectInput("Path_Specific","Know Loss Abnormality",c("Custom"="V_CL"))}
      if(region=="Pelvis"){out<-selectInput("Path_Specific","Know Loss Abnormality",c("Custom"="P_CL","Enlarged nutrient foramina"="P_L01", "Circular or linear depression(s) on the dorsal surface of the pubic symphyses"="P_L03"))}
      if(region=="Shoulder"){out<-selectInput("Path_Specific","Know Loss Abnormality",c("Custom"="SH_CL"))}
      if(region=="Leg"){out<-selectInput("Path_Specific","Know Loss Abnormality",c("Custom"="L_CL"))}
      if(region=="Foot"){out<-selectInput("Path_Specific","Know Loss Abnormality",c("Custom"="F_CL","Enlarged nutrient foramen"="F_L02"))}
      if(region=="Arm"){out<-selectInput("Path_Specific","Know Loss Abnormality",c("Custom"="A_CL"))}
      if(region=="Hand"){out<-selectInput("Path_Specific","Know Loss Abnormality",c("Custom"="H_CL"))}
      if(region=="Thorax"){out<-selectInput("Path_Specific","Know Loss Abnormality",c("Custom"="T_CL"))}}
    if(Type=="Formation"){
      if(region=="Skull"){out<-selectInput("Path_Specific","Know Formation Abnormality",c("Custom"="SK_CF"))}
      if(region=="Vertebrae"){out<-selectInput("Path_Specific","Know Formation Abnormality",c("Custom"="V_CF"))}
      if(region=="Pelvis"){out<-selectInput("Path_Specific","Know Formation Abnormality",c("Custom"="P_CF"))}
      if(region=="Shoulder"){out<-selectInput("Path_Specific","Know Formation Abnormality",c("Custom"="SH_CF"))}
      if(region=="Leg"){out<-selectInput("Path_Specific","Know Formation Abnormality",c("Custom"="L_CF"))}
      if(region=="Foot"){out<-selectInput("Path_Specific","Know Formation Abnormality",c("Custom"="F_CF"))}
      if(region=="Arm"){out<-selectInput("Path_Specific","Know Formation Abnormality",c("Custom"="A_CF"))}
      if(region=="Hand"){out<-selectInput("Path_Specific","Know Formation Abnormality",c("Custom"="H_CF"))}
      if(region=="Thorax"){out<-selectInput("Path_Specific","Know Formation Abnormality",c("Custom"="T_CF"))}}
    if(Type=="Shape"){
      if(region=="Skull"){out<-selectInput("Path_Specific","Know Shape/Size Abnormality",c("Custom"="SK_CS","Abnormally small circumference (Microscephly)"="SK_S01", "Abnormally large/wide cranium (Macrocephly)"="SK_S02", "Anterior Posterior elongation (Scaphocephaly)"="SK_S03", "Superior-Inferior elongation (Oxycephaly)"="SK_S04", "Flattening of the posterior skull(Brachycephaly)"="SK_S05", "Asymmetrical distortion(Plagiocephaly)"="SK_S06", "Anteriorly Pointed forehead (Trigonocephaly)"="SK_S07", "Asymmetry of the cranial base"="SK_S08", "Supernummery bones/sutural ossicles"="SK_S09", "Unfused Metopic Suture"="SK_S10", "Deviation from midline of nasal septum"="SK_S11", "Cleft palate"="SK_S12", "Suprainion depression"="SK_S13", "Absence/ complete closure of the external auditory meatus"="SK_S14"))}
      if(region=="Vertebrae"){out<-selectInput("Path_Specific","Know Shape/Size Abnormality",c("Custom"="V_CS","Supernummery vertebrae"="V_S10"))}
      if(region=="Pelvis"){out<-selectInput("Path_Specific","Know Shape/Size Abnormality",c("Custom"="P_CS"))}
      if(region=="Shoulder"){out<-selectInput("Path_Specific","Know Shape/Size Abnormality",c("Custom"="SH_CS"))}
      if(region=="Leg"){out<-selectInput("Path_Specific","Know Shape/Size Abnormality",c("Custom"="L_CS"))}
      if(region=="Foot"){out<-selectInput("Path_Specific","Know Shape/Size Abnormality",c("Custom"="F_CS","Accessory/supernummery bones"="F_S01","Fusion of interphalangeal joints"="F_S05","Fusion of  phalangeal metatarsal joints"="F_S06"))}
      if(region=="Arm"){out<-selectInput("Path_Specific","Know Shape/Size Abnormality",c("Custom"="A_CS"))}
      if(region=="Hand"){out<-selectInput("Path_Specific","Know Shape/Size Abnormality",c("Custom"="H_CS","Fusion of the proximal interphalangeal joint, resulting in a swan-neck deformity"="H_S01","Acessory/Supernummery bones"="H_S02"))}
      if(region=="Thorax"){out<-selectInput("Path_Specific","Know Shape/Size Abnormality",c("Custom"="T_CL","Accessory/supernummery ribs"="T_S03"))}}
  }
  out
}#Creates Path_Specfic based on Path_Type and region

Path2<-function(Type,Region,Bone,LID){
  if(length(Type)>1){Type<-"Complex"}
  uicreator<-paste0(LID,"_UI")
  path<-paste0("Options/Path/",Region,Type,".R")
  source(path,local=TRUE)
  out<-tagList(fixedPage(
    column(width=4,textInput("Path_Free","Overall Description",value=NA)),
    column(width=4,selectInput("Path_Heal","Healing state",c("Active","Healing","Healed","Unknown","NA"))),
    column(width=4,textInput("Path_Heal2","Reason for healing state determination",value="NA"))),
    fixedPage(get(uicreator)),
    h1(br()),h1(br()),h1(br()),h1(br()),h1(br()))
  out
}#Creates the Path_input

#Link####
Link1<-function(directory,file,individual){
  filepath<-paste(directory,file,".A.OD.txt",sep="")
  if(!(file.exists(filepath))){stop(filepath," Does not exist please check directory and population ID")}
  Table<-ReadBioArch(filepath)$Table
  if(individual!="ShowAll"){Table<-Table[Table$SID==individual,]}
  IDs<-paste0(Table$LID,"(",Table$D,")")
  LinkUI<-tagList(selectInput("LinkID","ID and date of connected lesion",multiple=TRUE,c("None",IDs)),
                  selectInput("LinkType","Connection type",multiple=TRUE,choice=c("Continuation","Symetrical","Overlapping","Similar appearance","Potential systemic condition","Shared joint","Additional description")),
                  textInput("LinkAdd","Additional Info",value="None"))
  list(UI=LinkUI,Table=Table)
}#creates UI output and table for Link Tab

#File####
CreateA<-function(ID,name,Investigator,dir){
  filepath<-paste(dir,ID,".A.OD.txt",sep="")
  if(file.exists(filepath)){stop(filepath," already exists")}
  Head<-c("#Osteological data:Abnormalities",
          paste("#Pop ID=",ID,":Pop Name=",name,":Investigator=",Investigator,sep=""),
          paste("#Date Created=",Sys.time(),":Program=SkeltalAbV1.0"),
          "#Fields:Skeletal ID,InvestigatorInitials,Date,Lesion ID,Type,Description 1,Description 2,Location,Features,Size,Nature,Healing,Linked lesions,Photographic",
          "#Type:Type of abnormality,Category:Trauma/Pathology/Taphnomic,(Chip/Hole/Puncture/Pit/Incision/ChopMark/Furrow/Impression/Depression/Fracture/Displacement/Ampuation/Other)/(Formation/Loss/Shape/Complex)/(CorticalFlaking/Burning/Discoluration/IncreasedMass/ReducedMass/Shrinkage/MineralDeposition/Inclusion/Other)",
          "#Des1:headline description:prescribed",
          "#Des2:user inputed description:free",
          "#Loc:Region,Bone(s),Additional:prescribed,prescribed,free",
          "#Feat:Features affected, sutures (skull only):prescribed",
          "#Size:names,values:headings,num(mm)",
          "#Nature:names,values:headings,prescribed/free",
          "#Healing:Stage,Evidence:(Active/Healing/Healed/unknown/NA)/(Post/Peri/Ante_Healing/Ante_Healed/unknown/NA),free",
          "#Link:IDS,connection,Additional:alphanumeric,prescribed,free",
          "#Photo:photographic reference info:filepath/ID")
  Table<-data.frame(SID=NA,In=NA,D=NA,LID=NA,Type=NA,Des1=NA,Des2=NA,Loc=NA,Feat=NA,Size=NA,Nature=NA,Heal=NA,Link=NA,photo=NA)
  write(Head,filepath)
  write.table(Table[-1,],filepath,append=TRUE,row.names=FALSE)
  paste("File ",ID,".A.OD.txt created", sep="")
}#creates new blank file with header

LocationTable<-function(input){
  Table<-data.frame(Loc=NA,Feat=NA)
  BonesList<-function(input){
    Bones<-input$Bone
    Unpaired<-Bones[Bones %in% c("Frontal","Occipital","Mandible","Ethmoid","Vomer","Hyoid","Sternum","Sacrum","Coccyx")]
    Unpaired<-paste(Unpaired,collapse=",")
    Paired<-Bones[!(Bones %in% c("Frontal","Occipital","Mandible","Ethmoid","Vomer","Hyoid","Sternum","Sacrum","Coccyx"))]
    if(length(Paired)>0){
      Sided<-NULL
    for(i in 1:length(Paired)){
      Side<-paste0(Paired[i],"_Side")
      side<-input[[Side]]
      Sided<-paste(Sided,paste0(Paired[i],side),collapse=",")}
      Bones<-paste(Unpaired,Paired,sep=",")
    }else{Bones<-Unpaired}
    Bones
    }
  Table$Loc<-paste(input$Region,BonesList(input),input$Location_Free,sep=":")
  if(input$Region=="Skull"){
    
  }else{Suture<-NA}
  
}#Creates the Loc and Feat fields using 

LinkTable<-function(input){
  if(input$show >=1 ){
  Ids<-paste(input$LinkID,collapse=",")
  Connection<-paste(input$LinkType,collapse=",")
  Additional<-FreeFix(input$LinkAdd)
  out<-paste(Ids,Connection,Additional,sep=":")
  }else{out<-"NA:NA:NA"}
  out
}#creates Link field

PathTable<-function(input){
  Table<-data.frame(SID=NA,In=NA,D=NA,LID=NA,Type=NA,Des1=NA,Des2=NA,Loc=NA,Feat=NA,Size=NA,Nature=NA,Heal=NA,Link=NA,photo=NA)
  #straight Input(always the same fields)
  Table$LID<-input$Path_Specific
  if(length(input$Path_Type)>1){Type<-paste0("Complex(",paste(input$Path_Type,collapse=","),")")}else{Type<-input$Path_Type}
  Table$Type<-paste("Pathology",Type,sep=":")
  Table$Des2<-input$Path_Free
  Table$Heal<-paste(input$Path_Heal,input$Path_Heal2,sep=":")
  #Size,Nature,Des1
  if(length(input$Path_Type)>1){Path<-paste0("Options/",input$Region,"Complex.R")}else{Path<-paste0("Options/",input$Region,input$Path_Type,".R")}
  source(path,local=TRUE)
  RecordCreate<-get(paste0(LID,"_RC"))
  RC<-RecordCreate(input)
  Table$Des1<-RC$Des1
  Table$Size<-RC$Size
  Table$Nature<-RC$Nature
  #out
  Table
}#Creates path table

TraumaTable<-function(input){
  Table<-data.frame(SID=NA,In=NA,D=NA,LID=NA,Type=NA,Des1=NA,Des2=NA,Loc=NA,Feat=NA,Size=NA,Nature=NA,Heal=NA,Link=NA,photo=NA)
  #From top field(same for all options)
  Table$Type<-paste("Trauma",paste(input$Trauma_Type,collapse=","),sep=":")
  Table$Des2<-input$Trauma_Free
  Table$Heal<-paste(input$Trauma_Heal,input$Trauma_Heal2,sep=":")
  #Size 
  SizeValues<-NULL;SizeName<-NULL
  for(i in 1:input$CustomMeasure){
    SizeValues<-c(SizeValues,input[[paste0("CMV_",i)]])
    SizeName<-c(SizeName,input[[paste0("CMN_",i)]])}
  Table$Size<-paste(paste(SizeName,collapse=","),paste(SizeValues,collapse=","),sep=":")
  #Nature and Des1
  NatureNames<-NULL;NatureValues<-NULL;Des1<-NULL
  for(i in 1:length(Input$Trauma_Type)){
    if(input$Trauma_Type[i]=="Fracture" && input$fract_Specific!="Custom"){
      source("Options/Fractures.R")
      RC<-get(paste0(input$fract_Specific,"_RC"))
    }else{
      source("Options/Trauma.R",local=TRUE)
      RC<-get(paste0(input$Trauma_Type[i],"_RC"))}
    RC<-RC(input)
    NatureNames<-c(NatureNames,RC$NatureNames)
    NatureValues<-c(NatureValues,RC$NatureValues)
    Des1<-c(Des1,RC$Des1)}
  Table$Nature<-paste(paste(NatureNames,collapse=","),paste(NatureValues,collapse=","),sep=":")
  Table$Des1<-paste(Des1,collapse=",")
  
  Table
}#Creates a trauma table

TaphTable<-function(input){
  Table<-data.frame(SID=NA,In=NA,D=NA,LID=NA,Type=NA,Des1=NA,Des2=NA,Loc=NA,Feat=NA,Size=NA,Nature=NA,Heal=NA,Link=NA,photo=NA)
  Table$Type<-paste("Taphonomic",paste(input$Taph_Type,collapse=","),sep=":")
  Table$Des2<-input$Taph_Free
  Table$Heal<-"Post:Taphonomic Damage"
  #Size 
  SizeValues<-NULL;SizeName<-NULL
  for(i in 1:input$CustomMeasure){
    SizeValues<-c(SizeValues,input[[paste0("CMV_",i)]])
    SizeName<-c(SizeName,input[[paste0("CMN_",i)]])}
  Table$Size<-paste(paste(SizeName,collapse=","),paste(SizeValues,collapse=","),sep=":")
  #Nature and Des1
  NatureNames<-NULL;NatureValues<-NULL;Des1<-NULL
  for(i in 1:length(Input$Taph_Type)){
    source("Options/Taph.R",local=TRUE)
    RC<-get(paste0(input$Taph_Type[i],"_RC"))
    RC<-RC(input)
    NatureNames<-c(NatureNames,RC$NatureNames)
    NatureValues<-c(NatureValues,RC$NatureValues)
    Des1<-c(Des1,RC$Des1)}
  Table$Nature<-paste(paste(NatureNames,collapse=","),paste(NatureValues,collapse=","),sep=":")
  Table$Des1<-paste(Des1,collapse=",")
  Table
}#creates taphonomy table


AppendA<-function(PopID,ID,In,dir,Type){
  #Check for file
  filepath<-paste(dir,PopID,".A.OD.txt",sep="")
  if(!(file.exists(filepath))){
    stop("File ",filepath," Does not exist please check the Population ID")
  }else{
    #Create table
    if(Type=="Pathology"){Table<-PathTable(input)}
    if(Type=="Trauma"){Table<-TraumaTable(input)}
    if(Type=="Taphonomic"){Table<-TaphTable(input)}
    Location<-LocationTable(input)
    Table$Loc<-Location$Loc
    Table$Feat<-Location$Feat
    Table$Link<-LinkTable(input)
    Table$Photo<-paste(input$Photo)
    Table$SID<-ID
    Table$In<-In
    Table$D<-format(Sys.time(),"%d/%m/%y_%H:%M:%S")
    #Write out
    write.table(Table,filepath,append=TRUE,row.names=FALSE,col.names=FALSE)
    paste("Added Individual",ID,"to",filepath)
  }
}#add record to existing file

CompCheck<-function(PopID,PopName,Investigator,dir){
  filepath<-paste(dir,PopID,".A.OD.txt",sep="")
  #Step1: check file exists
  if(!(file.exists(filepath))){
    stop("File ",filepath," Does not exist please check the Population ID")
  }else{
    #Extract data from header lines
    file<-readLines(filepath,n=17)
    line2<-file[2];line2<-strsplit(line2,split=":")
    Name<-strsplit(line2[[1]][2],split="=")[[1]][2]
    Invest<-strsplit(line2[[1]][3],split="=")[[1]][2]
    line3<-file[3];line3<-strsplit(line3,split=":")
    version<-tail(line3[[1]],1)
    
    #compatability warnings
    if(!(identical(Name,PopName))){print(paste("The Population names differ.In the existing file it is given as ",Name,sep=""))}
    if(!(identical(Invest,Investigator))){print(paste("The file was created by ",Invest,". Please make sure you have permission to write to this file",sep=""))}
    if(!(identical(version,"Program=SkeltalAbV1.0"))){print(paste("The file was created by using a diffrent version of this software, Please check version update info for potential compatability issues.The current version is SkeltalAbV1.0, the file was created using",version))}
    
  }
  print("Compatability check complete")
}#checks to see if details in an existing file match those current on the form