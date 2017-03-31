#functions used to construct the custom lesions tab inputs
make<-function(Add,input){
  if(Add==1){
    out<-tagList(fixedPage(h3(paste0("ID:C_",Add)),
                           column(width=4,
                                  h4("Location"),
                                  selectInput(paste0("region_",Add),"Region",c("Skull","Vertebrae","Pelvis","Shoulder","Thorax","Arm","Hand","Leg","Foot","Other/supernumerary"="Other")),
                                  renderUI(Bone(Add,bone=input[[paste0("region_",Add)]])),
                                  renderUI(Bone(Add,bone=input[[paste0("bone_",Add)]])),
                                  textInput(paste0("location_",Add),"Additional description of location"),
                                  h1(br()),
                                  h1(br()),
                                  h1(br())),
                           column(width=4,
                                  h4("Appearance"),
                                  selectInput(paste0("type_",Add),"Type",c("Bone loss"="Loss","Bone formation"="Formation","Combination of loss and formation"="Complex","Shape/Size Abnormality"="Shape")),
                                  renderUI(lesion1(Add,Type=input[[paste0("type_",Add)]],input)),
                                  textInput(paste0("appearance_",Add),"Additional description of appearance"),
                                  h1(br()),
                                  h1(br()),
                                  h1(br())),
                           column(width=4,h4("Connection with other lesions"),
                                  textInput(paste0("link1_",Add),"ID of linked lesion(s)",value="None"),
                                  selectizeInput(paste0("link2_",Add),"Connection type",multiple=TRUE,choice=c("Continuation","Symetrical","Overlapping","Similar appearance","Potential systemic condition","Shared joint","Additional description"))
                           )))
  }else{
    out<-tagList(fixedPage(h3(paste0("ID:C_",Add)),
                           column(width=4,
                                  h4("Location"),
                                  selectInput(paste0("region_",Add),"Region",c("Skull","Vertebrae","Pelvis","Shoulder","Thorax","Arm/Hand","Leg/Foot","Other/supernumerary"="Other")),
                                  renderUI(Bone(Add,bone=input[[paste0("region_",Add)]])),
                                  renderUI(Bone(Add,bone=input[[paste0("bone_",Add)]])),
                                  textInput(paste0("location_",Add),"Additional description of location")),
                           column(width=4,
                                  h4("Appearance"),
                                  selectInput(paste0("type_",Add),"Type",c("Bone loss"="Loss","Bone formation"="Formation","Combination of loss and formation"="Complex","Shape/Size Abnormality"="Shape")),
                                  renderUI(lesion1(Add,Type=input[[paste0("type_",Add)]],input)),
                                  textInput(paste0("appearance_",Add),"Additional description of appearance")),
                           column(width=4,h4("Connection with other lesions"),
                                  textInput(paste0("link1_",Add),"ID of linked lesion(s)",value="None"),
                                  selectizeInput(paste0("link2_",Add),"Connection type",multiple=TRUE,choice=c("Continuation","Symetrical","Overlapping","Similar appearance","Potential systemic condition","Shared joint","Additional description"))
                                  
                           )))
    
  }
  out
}

Bone<-function(Add,bone){
  #list of features for each bone
  out<-br()
  if(bone=="Skull"){out<-selectInput(paste0("bone_",Add),"Bone",c("Frontal","Parietal","Occipital","Zygomatic","Temporal","Maxilla","Mandible","Palatine","Sphenoid","Nasal","Lacrimal","Ethmoid","Vomer","Inferior nasal cochnea"="Cochnea","Hyoid"))}
  if(bone=="Vertebrae"){out<-selectInput(paste0("bone_",Add),"Vertebral region",c("Atlas","Axis","Cervical 3-7"="Cervical","Thoracic","Lumbar","Sacrum"))}
  if(bone=="Pelvis"){out<-selectInput(paste0("bone_",Add),"Bone",c("Os coxa"="OsCoxa","Sacrum","Coccyx"))}
  if(bone=="Shoulder"){out<-selectInput(paste0("bone_",Add),"Bone",c("Scapula","Clavicle"))}
  if(bone=="Leg"){out<-selectInput(paste0("bone_",Add),"Bone",c("Femur","Patella","Tibia","Fibula"))}
  if(bone=="Foot"){out<-selectInput(paste0("bone_",Add),"Bone",c("Talus","Calcaneous","Navicular","Cuboid","Cuniform","Metatarsal","Phalanx"))}
  if(bone=="Arm"){out<-selectInput(paste0("bone_",Add),"Bone",c("Humerus","Radius","Ulna"))}
  if(bone=="Hand"){out<-selectInput(paste0("bone_",Add),"Bone",c("Scaphoid","Hamate","Capitate","Lunate","Pisiform","Triquetral","Trapezium","Trapezoid","Metacarpal","Phalanx"))}
  if(bone=="Thorax"){out<-selectInput(paste0("bone_",Add),"Bone",c("R1","R2","R3","R4","R5","R6","R7","R8","R9","R10","R11","R12","Sternum"))}
  if(bone=="Other"){out<-selectInput(paste0("bone_",Add),"Bone",c("supernumerary vertebra"="snum_Vert","supernumerary rib"="snum_Rib","supernumerary foot bones"="snum_Foot","supernumerary hand bones"="snum_Hand","Sutural ossicle"="snum_Sutural","other supernummary bones"="snum_Other"))}
  
  if(bone=="Frontal"){out<-tagList(selectInput(paste0("surface_",Add),"Surface",c("Endocranial","Ectocranial")),
                                   selectizeInput(paste0("feature_",Add),"Features(select all affected)",multiple=TRUE,
                                                  choice=c("pars orbitalia left"="pars_orbitalia_l","pars orbitalia right"="pars_orbitalia_r","ethmoid notch"="ethmoid_not","frontal sinus left"="sinus_l","frontal sinus right"="sinus_r",
                                                           "supraorbital margin left"="supraorbital_margin_l","supraorbital margin right"="supraorbital_margin_r","glabella","frontal eminence left"="emin_l","frontal eminence right"="emin_r",
                                                           "frontal Squama"="frontal_squama","temporal line left"="temporal_line_l","temporal line right"="temporal_line_r","zygomatic process left"="zygomatic_pro_l","zygomatic process right"="zygomatic_pro_r")),
                                   selectizeInput(paste0("suture_",Add),"Sutures(select all affected)",multiple=TRUE,
                                                  choice=c("coronal","right frontonasal"="frontonasal_r","left frontonasal"="frontonasal_l","frontosphenoid","right zygomaticofrontal"="zygomaticofrontal_r","left zygomaticofrontal"="zygomaticofrontal_l","right frontomaxillary"="frontomaxillary_r","left frontomaxillary"="frontomaxillary_l",
                                                           "right frontolacrimal"="frontolacrimal_r","left frontolacrimal"="frontolacrimal_l","frontoethmoidal","metopic","right frontotemporal"="frontotemporal_r","left frontotemporal"="frontotemporal_l")))}
  
  if(bone=="Parietal"){out<-tagList(selectInput(paste0("side_",Add),"Side",c("Unknown"="","Right"="_r","Left"="_l")),
                                    selectInput(paste0("surface_",Add),"Surface",c("Endocranial","Ectocranial")),
                                    selectizeInput(paste0("feature_",Add),"Feature (select all affected)",multiple=TRUE,
                                                   choice=c("parietal striae"="parietal_striae","inferior temporal line"="inf_temporal_line","superior temporal line"="sup_temporal_line","parietal eminence"="emin","sagittal sulcus"="sagittal_sul","sigmoid sulcus"="sigmoid_sul","parietal foramina"="parietal_mina")),
                                    selectizeInput(paste0("suture_",Add),"Sutures(select all affected)",multiple=TRUE,
                                                   choice=c("coronal","saggital","lambdoid","squamosal","parietomastoid","sphenoparietal")))}
  
  if(bone=="Occipital"){out<-tagList(selectInput(paste0("surface_",Add),"Surface",c("Endocranial","Ectocranial")),
                                     selectizeInput(paste0("feature_",Add),"Feature (select all affected)",multiple=TRUE,
                                                    choice=c("occipital planum"="occipital_planum","external occipital protuberance"="ex_protub","superior nuchal line"="sup_nuchal_line","inferior nuchal line"="inf__nuchal_line","nuchal planum"="nuchal_planum","external occipital crest"="ex_cr",
                                                             "cruciform eminence"="cruciform_emin","transverse sulcus"="transverse_sul","cerebral fossa left"="cerebral_fos_l","cerebral fossa right"="cerebral_fos_r","occipital sulcus"="occipital_sul","cerebellar fossa right"="cerebellar_fos_r","cerebellar fossa left"="cerebellar_fos_l","interal occipital crest"="in_cr",
                                                             "occipital condyle right"="con_r","occipital condyle left"="con_l","condylar fossa left"="condylar_fos_l","condylar fossa right"="condylar_fos_r","condylar foramen left"="condylar_for_l","condylar foramen right"="condylar_for_r","hypoglossal canal left"="hypoglossal_can_l","hypoglossal canal right"="hypoglossal_can_r",
                                                             "jugular process left"="jugular_pro_l","jugular process right"="jugular_pro_r","jugular notch left"="jugular_not_l","jugular notch right"="jugular_not_l","formen magnum border"="for_magnum_border","basilar part"="basilar")),
                                     selectizeInput(paste0("suture_",Add),"Sutures(select all affected)",multiple=TRUE,
                                                    choice=c("lambdoidal","right occipitomastoid"="occipitomastoid_r","left occipitomastoid"="occipitomastoid_l","right petro-occipital"="petrooccipital_r","left petro-occipital","petrooccipital_l","spheno-occipital synchondrosis"="synchondrosis")))}
  
  if(bone=="Zygomatic"){out<-tagList(selectInput(paste0("side_",Add),"Side",c("Unknown"="","Right"="_r","Left"="_l")),
                                     selectInput(paste0("surface_",Add),"Surface",c("Endocranial","Ectocranial")),
                                     selectizeInput(paste0("feature_",Add),"Feature (select all affected)",multiple=TRUE,
                                                    choice=c("frontal process"="frontal_pro","temporal process"="temporal_pro","maxillary process"="maxillary_pro","zygomaticofacial foramen"="zygomaticofacial_for","masseteric origin"="masseteric","zygomaticotemporal foramen"="zygomaticotemporal_for","orbital surface"="orbit","zygomaticoorbital foramina"="zygomaticoorbital_mina")),
                                     selectizeInput(paste0("suture_",Add),"Sutures(select all affected)",multiple=TRUE,
                                                    choice=c("zygomaticofrontal","zygomaticotemporal","zygomaticomaxillary","sphenozygomatic")))}
  
  if(bone=="Temporal"){out<-tagList(selectInput(paste0("side_",Add),"Side",c("Unknown"="","Right"="_r","Left"="_l")),
                                    selectInput(paste0("surface_",Add),"Surface",c("Endocranial","Ectocranial")),
                                    selectizeInput(paste0("feature_",Add),"Feature (select all affected)",multiple=TRUE,
                                                   choice=c("squama","petrous pyramid"="petrous","external auditory meatus"="ex_auditory_mea","zygomatic process"="zygomatic_p","suprameatal crest"="suprameatal_cr","supramastoid crest"="supramastoid_cr","mastoid process"="mastoid_pro","internal auditory meatus"="in_auditory_mea","sigmoid sulcus"="sigmoid_sul","parietal notch"="parietal_not",
                                                            "mastoid notch"="mastoid_not","jugular fossa"="jugular_fos","mandibular fossa"="mandibular_fos","occipital sulcus"="occipital_sul","styloid process"="styloid","articular eminence"="articular_emin","entoglenoid process"="entoglenoid_pro","tympanic part"="tympanic","vaginal process"="vaginal_pro","carotid canal"="carotid_can","stylomastoid foramen"="stylomastoid_for")),
                                    selectizeInput(paste0("suture_",Add),"Sutures(select all affected)",multiple=TRUE,
                                                   choice=c("squamosal","occipitomastoid","sphenosquamosal","zygomaticotemporal","petro-occipital"="petrooccipital","petrosquamosal","Frontotemporal")))}
  
  if(bone=="Maxilla"){out<-tagList(selectInput(paste0("side_",Add),"Side",c("Unknown"="","Right"="_r","Left"="_l")),
                                   selectInput(paste0("surface_",Add),"Surface",c("Endocranial","Ectocranial")),
                                   selectizeInput(paste0("feature_",Add),"Feature (select all affected)",multiple=TRUE,
                                                  choice=c("alveolar process"="alveolar_pro","frontal process"="frontal_pro","zygomatic process"="zygomatic_pro","infraorbital foramen"="infraorbital_for","canine fossa"="canine_fos","naso-alveolar clivus"="nasoalveolar_clivus","palatine process"="palatine_pro",
                                                           "maxillary sinus"="maxillary_sinus","infraorbital sulcus"="infraorbital_sul","anterior lacrimal crest"="ant_lacrimal_cr","anterior nasal spine"="ant_nasal_sp","canine jugum"="canine_jugum","greater palatine groove"="great_palatine_gr","incisive foramen"="incisive_for")),
                                   selectizeInput(paste0("suture_",Add),"Sutures(select all affected)",multiple=TRUE,
                                                  choice=c("frontomaxillary","zygomaticomaxillary","nasomaxillary","intermaxillary","palatomaxillary","ethmoidomaxillary","lacromaxillary","vomer-maxillary"="vomermaxillary","conchal-maxillary"="conchalmaxillary","sphenomaxillary")))}
  
  if(bone=="Mandible"){out<-tagList(selectInput(paste0("surface_",Add),"Surface",c("Internal","External")),
                                    selectizeInput(paste0("feature_",Add),"Feature (select all affected)",multiple=TRUE,
                                                   choice=c("corpus","metal foramen right"="mental_for_r","mental formaen left"="mental_for_l","oblique line right"="oblique_line_r","oblique line left"="oblique_line_l","mental protuberance"="mental_protub","masseteric tuberosity right"="masseteric_tub_r","masseteric tuberosity left"="masseteric_tub_l","masseteric fossa right"="masseteric_fos_r","masseteric fossa left"="masseteric_fos_l",
                                                            "ramus right"="ramus_r","ramus left"="ramus_l","coronoid process right"="coronoid_pro_r","coronoid process left"="coronoid_pro_l","mandibular notch right"="mandibular_not_r","mandibular notch left"="mandibular_not_l","condylar neck right"="condylar_neck_r","condylar neck left"="condylar_neck_l","mandibular condyle right"="mandibular_con_r","mandibular condyle left"="mandibular_con_l",
                                                            "mandibular foramen right"="mandibular_for_r","mandibular foramen left"="mandibular_for_l","lingula right"="lingula_r","lingula left"="lingula_l","mylohoid groove right"="mylohoid_gr_r","mylohoid groove left"="mylohoid_gr_l","pterygoid tuberosities right"="pterygoid_tub_r","pterygoid tuberosies left"="pterygoid_tub_l","mylohyoid line right"="mylohyoid_line_r","mylohyoid line left"="mylohyoid_line_l",
                                                            "submandibular fossa right"="submandibular_fos_r","submandibular fossa left"="submandibular_fos_l","mental spines"="mental_sps","digastric fossa"="digastric_fos","sublingual fossa"="sublingual_fos","endocoronoid ridge right"="endocoronoid_rdg_r","endocoronoid ridge left"="endocoronoid_rdg_l","extramolar sulcus right"="extramolar_sul_r","extramolar sulcus left"="extramolar_sul_l")))}
  
  if(bone=="Palatine"){out<-tagList(selectInput(paste0("side_",Add),"Side",c("Unknown"="","Right"="_r","Left"="_l")),
                                    selectizeInput(paste0("feature_",Add),"Feature (select all affected)",multiple=TRUE,
                                                   choice=c("horizontal plate"="horizontal_plate","greater palatine foramen"="great_palatine_for","pterygopalatine canal"="pterygopalatine_can","posterior nasal spine"="pos_nasal_sp","lesser palatine foramina"="less_palatine_mina","perpendicular plate"="perpendicular_plate")),
                                    selectizeInput(paste0("suture_",Add),"Sutures(select all affected)",multiple=TRUE,
                                                   choice=c("palatomaxillary","interpalatine","sphenopalatine","vomer-palatine"="vomerpalatine","conchal-palatine"="conchalpalatine","palatoethmoidal")))}
  
  if(bone=="Sphenoid"){out<-tagList(selectizeInput(paste0("feature_",Add),"Feature (select all affected)",multiple=TRUE,
                                                   choice=c("corpus","optic canal right"="optic_can_r","optic canal left"="optic_can_l","sella turcica"="sella_turcica","hypophyseal fossa"="hypophyseal_fos","dorsum sellae","posterior clinoid process"="pos_clinoid_pro","clivus","sphenoidal sinus right"="sinus_r","sphenoidal sinus left"="sinus_l","sphenoidal rostrum"="rostrum","sphenoidal crest"="sphenoidal_cr",
                                                            "greater wing right"="great_wing_r","greater wing left"="great_wing_l","superior orbital fissure right"="sup_orbital_fiss_r","superior orbital fissure left"="sup_orbital_fiss_l","foramen rotundum right"="for_rotundum_r","foramen rotundum left"="for_rotundum_l","foramen ovale right"="for_ovale_r","foramen ovale left"="for_ovale_l","foramen spinosum right"="for_spinosum_r","foramen spinosum left"="for_spinosum_l",
                                                            "infratemporal crest right"="infratemporal_cr_r","infratemporal crest left"="infratemporal_cr_l","orbital surface right"="orbital_face_r","orbital surface left"="orbital_face_l","lesser wing right"="less_wing_r","lesser_wing_l"="less_wing_r","anterior clinoid process right"="ant_clinoid_pro_r","anterior clinoid process left"="ant_clinoid_pro_l",
                                                            "pterygoid process right"="pterygoid_pro_r","pterygoid process left"="pterygoid_pro_l","lateral pterygoid plate right"="lat_pterygoid_plate_r","lateral pterygoid plate left"="lat_pterygoid_plate_l","medial pterygoid plate right"="med_pterygoid_plate_r","medial pterygoid plate left"="med_pterygoid_plate_l","pterygoid fossa right"="pterygoid_fos_right","pterygoid fossa left"="pterygoid_fos_left",
                                                            "pterygoid hamulus right"="pterygoid_ham_r","pterygoid hamulus left"="pterygoid_ham_l","pterygoid canal right"="pterygoid_can_r","pterygoid canal left"="pterygoid_can_l")),
                                    selectizeInput(paste0("suture_",Add),"Sutures(select all affected)",multiple=TRUE,
                                                   choice=c("sphenofrontal","sphenosquamosal right"="sphenosquamosal_r","sphenosquamosal left"="sphenosquamosal_l","sphenozygomatic right"="sphenozygomatic_r","sphenozygomatic left"="sphenozygomatic_l","spheno-occipital synchondrosis"="synchondrosis","sphenoparietal right"="sphenoparietal_r","sphenoparietal left"="sphenoparietal_l",
                                                            "sphenopalatine right"="sphenopalatine_r","sphenopalatine left"="sphenopalatine_l","sphenomaxillary right"="sphenomaxillary_r","sphenomaxillary left"="sphenomaxillary_l","sphenovomerian","sphenoethmoidal")))}
  
  if(bone=="Nasal"){out<-tagList(selectInput(paste0("side_",Add),"Side",c("Unknown"="","Right"="_r","Left"="_l")),
                                 selectInput(paste0("surface_",Add),"Surface",c("Endocranial","Ectocranial")),
                                 selectizeInput(paste0("suture_",Add),"Sutures(select all affected)",multiple=TRUE,
                                                choice=c("frontonasal","nasomaxillary","internasal","nasoethmodial")))}
  
  if(bone=="Lacrimal"){out<-tagList(selectInput(paste0("side_",Add),"Side",c("Unknown"="","Right"="_r","Left"="_l")),
                                    selectInput(paste0("surface_",Add),"Surface",c("Endocranial","Ectocranial")),
                                    selectizeInput(paste0("suture_",Add),"Sutures(select all affected)",multiple=TRUE,
                                                   choice=c("lacrimoconchal","frontolacrimal","ethmoidolacrimal","lacromaxillary")))}
  
  if(bone=="Ethmoid"){out<-tagList(selectizeInput(paste0("feature_",Add),"Feature (select all affected)",multiple=TRUE,
                                                  choice=c("cribiform plate"="cribiform_plate","crista galli"="crista_galli","right labyrinth"="labyrinth_r","left labyrinth"="labyrinth_l")),
                                   selectizeInput(paste0("suture_",Add),"Sutures(select all affected)",multiple=TRUE,
                                                  choice=c("frontoethmoidal","ethmoidomaxillary right"="ethmoidomaxillary_r","ethmoidomaxillary left"="ethmoidomaxillary_l","ethmoidolacrimal right"="ethmoidolacrimal_r","ethmoidolacrimal left"="ethmoidolacrimal_l","palatoethmoidal right"="palatoethmoidal_r","palatoethmoidal left"="palatoethmoidal_l",
                                                           "ethmoidovomerian","ethmoidoconchal right"="ethmoidoconchal_r","ethmoidoconchal left"="ethmoidoconchal_l","nasoethmodial right"="nasoethmodial_r","nasoethmodial left"="nasoethmodial_l","sphenoethmoidal")))}
  if(bone=="Vomer"){out<-selectizeInput(paste0("suture_",Add),"Sutures(select all affected)",multiple=TRUE,
                                        choice=c("sphenovomerian","ethmoidovomerian","vomer-maxillary left"="vomermaxillary_l","vomer-maxillary right"="vomermaxillary_r","vomer-palatine right"="vomerpalatine_r","vomer-palatine left"="vomerpalatine_l"))}
  
  if(bone=="Cochnea"){out<-tagList(selectInput(paste0("side_",Add),"Side",c("Unknown"="","Right"="_r","Left"="_l")),
                                   selectInput(paste0("surface_",Add),"Surface",c("Endocranial","Ectocranial")),
                                   selectizeInput(paste0("suture_",Add),"Sutures(select all affected)",multiple=TRUE,
                                                  choice=c("lacrimoconchal","conchal-maxillary"="conchalmaxillary","conchal-palatine"="conchalpalatine","ethmoidoconchal")))}
  
  if(bone=="Hyoid"){out<-selectizeInput(paste0("feature_",Add),"Feature (select all affected)",multiple=TRUE,
                                        choice=c("body","right lesser horn"="less_horn_r","left lesser horn"="less_horn_l","right greater horn"="great_horn_r","left greater horn"="great_horn_l"))}
  if(bone=="Atlas"){out<-selectizeInput(paste0("feature_",Add),"Feature(select all affected)",multiple=TRUE,
                                        choice=c("left pedicle"="pedicle_l","right pedicle"="pedicle_r","vertebral arch right side"="arch_r","vertebral arch left side"="arch_l",
                                                 "right lamina"="lamina_r","left lamina"="lamina_l","right transverse process"="transverse_pro_r","left transverse process"="transvers_pro_l","right superior articular facet"="sup_artical_fac_r","left superior articular facet"="sup_articular_fac_l",
                                                 "right inferior articular facet"="inf_articular_fac_r","left inferior articular facet"="inf_articular_fac_l","right transverse foramen"="transverse_for_r","left transverse foramen"="transverse_for_l","odontoid facet"="odontoid_fac"))}
  if(bone=="Axis"){out<-selectizeInput(paste0("feature_",Add),"Feature(select all affected)",multiple=TRUE,
                                       choice=c("spinous process"="spinous_pro","left pedicle"="pedicle_l","right pedicle"="pedicle_r","vertebral arch right side"="arch_r","vertebral arch left side"="arch_l",
                                                "right lamina"="lamina_r","left lamina"="lamina_l","right transverse process"="transverse_pro_r","left transverse process"="transvers_pro_l","right superior articular facet"="sup_artical_fac_r","left superior articular facet"="sup_articular_fac_l",
                                                "right inferior articular facet"="inf_articular_fac_r","left inferior articular facet"="inf_articular_fac_l","right transverse foramen"="transverse_for_r","left transverse foramen"="transverse_for_l","odontoid process"="odontoid_pro"))}
  if(bone=="Cervical"){out<-tagList(selectizeInput(paste0("bones_",Add),"Vertebrae(select all affected)",multiple=TRUE,choice=c("C3","C4","C5","C6","C7")),
                                    selectizeInput(paste0("feature_",Add),"Feature(select all affected)",multiple=TRUE,
                                                   choice=c("body","spinous process"="spinous_pro","left pedicle"="pedicle_l","right pedicle"="pedicle_r","vertebral arch right side"="arch_r","vertebral arch left side"="arch_l",
                                                            "right lamina"="lamina_r","left lamina"="lamina_l","right transverse process"="transverse_pro_r","left transverse process"="transvers_pro_l","right superior articular facet"="sup_artical_fac_r","left superior articular facet"="sup_articular_fac_l",
                                                            "right inferior articular facet"="inf_articular_fac_r","left inferior articular facet"="inf_articular_fac_l","right transverse foramen"="transverse_for_r","left transverse foramen"="transverse_for_l")))}
  if(bone=="Thoracic"){out<-tagList(selectizeInput(paste0("bones_",Add),"Vertebrae(select all affected)",multiple=TRUE,choice=c("T1","T2","T3","T4","T5","T6","T7","T8","T9","T10","T11","T12")),
                                    selectizeInput(paste0("feature_",Add),"Feature(select all affected)",multiple=TRUE,
                                                   choice=c("body","spinous process"="spinous_pro","left pedicle"="pedicle_l","right pedicle"="pedicle_r","vertebral arch right side"="arch_r","vertebral arch left side"="arch_l",
                                                            "right lamina"="lamina_r","left lamina"="lamina_l","right transverse process"="transverse_pro_r","left transverse process"="transvers_pro_l","right superior articular facet"="sup_artical_fac_r","left superior articular facet"="sup_articular_fac_l","right inferior articular facet"="inf_articular_fac_r","left inferior articular facet"="inf_articular_fac_l",
                                                            "right inferior costal demifacet"="inf_demi_r","left inferior costal demi-facet"="inf_demi_l","right superior costal demi-facet"="sup_demi_r","left superior costal demi-facet"="sup_demi_l","right transverse process costal facet"="transverse_fac_r","left transverse process costal facet"="transverse_fac_l","Right vertebral body facet"="body_fac_r","Left vertebral body facet"="body_fac_l")))}
  if(bone=="Lumbar"){out<-tagList(selectizeInput(paste0("bones_",Add),"Vertebrae(select all affected)",multiple=TRUE,choice=c("L1","L2","L3","L4","L5")),
                                  selectizeInput(paste0("feature_",Add),"Feature(select all affected)",multiple=TRUE,
                                                 choice=c("body","spinous process"="spinous_pro","left pedicle"="pedicle_l","right pedicle"="pedicle_r","vertebral arch right side"="arch_r","vertebral arch left side"="arch_l",
                                                          "right lamina"="lamina_r","left lamina"="lamina_l","right transverse process"="transverse_pro_r","left transverse process"="transvers_pro_l","right superior articular facet"="sup_artical_fac_r","left superior articular facet"="sup_articular_fac_l",
                                                          "right inferior articular facet"="inf_articular_fac_r","left inferior articular facet"="inf_articular_fac_l")))}
  if(bone %in% c("R3","R4","R5","R6","R7","R8","R9","R10","R11","R12")){out<-tagList(selectInput(paste0("side_",Add),"Side",c("Unknown"="","Right"="_r","Left"="_l")),
                                                                                     selectizeInput(paste0("feature_",Add),"Feature(select all affected)",multiple=TRUE,
                                                                                                    choice=c("head","neck","tubercle","angle","shaft","costal groove"="costal_gr","sternal end"="sternal_end","cranial edge"="cranial_edge","caudal edge"="caudal_edge")))}
  if(bone=="R1"){out<-tagList(selectInput(paste0("side_",Add),"Side",c("Unknown"="","Right"="_r","Left"="_l")),
                              selectizeInput(paste0("feature_",Add),"Feature(select all affected)",multiple=TRUE,
                                             choice=c("head","neck","tubercle","angle","shaft","sternal end"="sternal_end","cranial edge"="cranial_edge","caudal edge"="caudal_edge","subclavian vein groove"="subclavian_gr","brachial plexus groove"="plexus_gr","scalene tubercle"="scalene_tubc")))}
  if(bone=="R2"){out<-tagList(selectInput(paste0("side_",Add),"Side",c("Unknown"="","Right"="_r","Left"="_l")),
                              selectizeInput(paste0("feature_",Add),"Feature(select all affected)",multiple=TRUE,
                                             choice=c("head","neck","tubercle","angle","shaft","costal groove"="costal_gr","sternal end"="sternal_end","cranial edge"="cranial_edge","caudal edge"="caudal_edge","serratus anterior tuberosity"="serratus_tub")))}
  if(bone=="Sternum"){out<-selectizeInput(paste0("feature_",Add),"Feature(select all affected)",multiple=TRUE,
                                          choice=c("manubrium","right clavicular notch"="clavicular_not_r","left clavicular notch"="clavicular_not_l","jugular notch"="jugular_not","corpus sterni"="corpus","xiphoid process"="xiphoid_pro","1st right costal notch"="costal1_not_r","1st left costal notch"="costal1_not_l","2nd right costal notch"="costal2_not_r","2nd left costal notch"="costal2_not_l","3rd right costal notch"="costal3_not_r","3rd left costal notch"="costal3_not_l",
                                                   "4th right costal notch"="costal4_not_r","4th left costal notch"="costal4_not_l","5th right costal notch"="costal5_not_r","5th left costal notch"="costal5_not_l","6th right costal notch"="costal6_not_r","6th left costal notch"="costal6_not_l","7th right costal notch"="costal7_not_r","7th left costal notch"="costal7_not_l"))}
  if(bone=="Clavicle"){out<-tagList(selectInput(paste0("side_",Add),"Side",c("Unknown"="","Right"="_r","Left"="_l")),
                                    selectizeInput(paste0("feature_",Add),"Feature(select all affected)",multiple=TRUE,
                                                   choice=c("costal tuberosity"="costal_tub","subclavian sulcus"="subclavian_sul","conoid tubercle"="conoid_tubc","trapezoid line"="trapezoid_line","superior surface"="sup_face","trapezius attachment"="trapezius_att","deltoideus attachment"="deltoideus_att","pectoralis major attachment"="pectoralis_att")))}
  if(bone=="Scapula"){out<-tagList(selectInput(paste0("side_",Add),"Side",c("Unknown"="","Right"="_r","Left"="_l")),
                                   selectizeInput(paste0("feature_",Add),"Feature(select all affected)",multiple=TRUE,
                                                  choice=c("coracoid process"="coracoid_pro","acromion process"="acromion_pro","glenoid fossa"="glenoid_fos","superior border"="sup_border","Scapula notch/foramen"="scapula_not","superior angle"="sup_angle","medial border"="med_border","inferior angle"="inf_angle","lateral border"="lat_border",
                                                           "subscapular fossa"="subscapular_fos","oblque ridges"="oblique_rdg","scapular spine"="scapular_sp","supraspinous fossa"="supraspinous_fos","infraspinous fossa"="infraspinous_fos","scapula neck"="neck","supraglenoid tubercle"="supraglenoid_tubc","infraglenoid tubercle"="infraglenoid_tubc")))}
  if(bone=="Humerus"){out<-tagList(selectInput(paste0("side_",Add),"Side",c("Unknown"="","Right"="_r","Left"="_l")),
                                   selectizeInput(paste0("feature_",Add),"Feature(select all affected)",multiple=TRUE,
                                                  choice=c("head","anatomical neck"="anatomical_neck","surgical neck"="surgical_neck","greater tubercle"="great_tubc","lesser tubercle"="less_tubc","intertubercular sulcus"="intertubercular_sul","greater tubercle crest"="great_tubc_cr","lesser tubercle crest"="less_tubc_cr",
                                                           "trochlea","capitulum","coronoid fossa"="coronoid_fos","radial fossa"="radial_fos","medial supracondylar crest"="med_supracondylar_cr","lateral supracondylar crest"="lat_supracondylar_cr","medial epicondyle"="med_epicondyle","lateral epicondyle"="lat_epicondyle","olecranon fossa"="olecranon_fos",
                                                           "shaft","nutrient foramen"="nutrient_for","deltoid tuberosity"="deltoid_tub","radial sulcus"="radial_sul")))}
  if(bone=="Radius"){out<-tagList(selectInput(paste0("side_",Add),"Side",c("Unknown"="","Right"="_r","Left"="_l")),
                                  selectizeInput(paste0("feature_",Add),"Feature(select all affected)",multiple=TRUE,
                                                 choice=c("head","neck","radial tuberosity"="radial_tub","shaft","nutrient foramen"="nutrient_for","interosseous crest"="interosseous_cr","oblique line"="oblique_line","pronator teres insersion"="pronator_att","ulnar notch"="ulna_not","distal articular surface"="dis_articular_face","styloid process"="styloid_pro","dorsal tubercle"="dorsal_tubc")))}
  if(bone=="Ulna"){out<-tagList(selectInput(paste0("side_",Add),"Side",c("Unknown"="","Right"="_r","Left"="_l")),
                                selectizeInput(paste0("feature_",Add),"Feature(select all affected)",multiple=TRUE,
                                               choice=c("olecranon process"="olecranon_pro","trochlear notch"="trochlear_not","guiding ridge"="guiding_rdg","coronoid process"="coronoid_pro","ulnar tuberosity"="ulna_tub","radial notch"="radial_not",
                                                        "shaft","nutrient foramen"="nutrient_for","interosseous crest"="interosseous_cr","pronator ridge"="pronator_rdg","styloid process"="styloid_pro","extensor carpi ulnaris groove"="ulnaris_gr","radial articulation"="radial_articulation")))}
  if(bone=="Phalanx"){out<-tagList(selectInput(paste0("side_",Add),"Side",c("Unknown"="","Right"="_r","Left"="_l")),
                                   selectInput(paste0("number_",Add),"Phalanx",c("proximal 1"="_p1","distal 1"="_d1","proximal 2"="_p2","intermediate 2"="_i2","distal 2"="_d2","proximal 3"="_p3","intermediate 3"="_i3","distal3"="_d3","proximal 4"="_p4","intermediate 4"="_i4","distal 4"="_d4","proximal 5"="_p5","intermediate 5"="_i5","distal 5"="_d5")),
                                   selectizeInput(paste0("feature_",Add),"Feature(select all affected)",multiple=TRUE,
                                                  choice=c("head","shaft","base")))}
  if(bone %in% c("Metacarpal","Metatarsal")){out<-tagList(selectInput(paste0("side_",Add),"Side",c("Unknown"="","Right"="_r","Left"="_l")),
                                                          selectInput(paste0("number_",Add),"Ray",c("1st"="_1","2nd"="_2","3rd"="_3","4th"="_4","5th"="_5")),
                                                          selectizeInput(paste0("feature_",Add),"Feature(select all affected)",multiple=TRUE,
                                                                         choice=c("head","shaft","base")))}
  if(bone %in% c("Scaphoid","Hamate","Capitate","Lunate","Pisiform","Triquetral","Trapezium","Trapezoid","Cuboid","Navicular")){out<-selectInput(paste0("side_",Add),"Side",c("Unknown"="","Right"="_r","Left"="_l"))}
  if(bone=="Sacrum"){out<-selectizeInput(paste0("feature_",Add),"Feature(select all affected)",multiple=TRUE,
                                         choice=c("promontory","right alae"="alae_r","left alae"="alae_l","right auricular surface"="auricular_face_r","left auricular surface"="auricular_face_l","right superior articular facte"="sup_articular_fac_r","left superior articular facte"="sup_articular_fac_l","dorsal wall"="dorsal_wall","median crest"="median_cr","right anterior sacral foramina"="ant_sacral_mina_r","left anterior sacral foramina"="ant_sacral_mina_l","transverse lines"="transverse_line"))}
  if(bone=="OsCoxa"){out<-tagList(selectInput(paste0("side_",Add),"Side",c("Unknown"="","Right"="_r","Left"="_l")),
                                  selectizeInput(paste0("feature_",Add),"Feature(select all affected)",multiple=TRUE,
                                                 choice=c("illium","ischium","pubis","acetabulum","acetabular fossa"="acetabular_fos","lunate surface"="lunate_face","iliac pilar"="illiac_pilar","iliac tubercle"="iliac_tubc","iliac crest"="iliac_cr","anterior gluteal line"="ant_gluteal_line","posterior gluteal line"="pos_gluteal_line","inferior gluteal line"="inf_gluteal_line","iliac fossa"="iliac_fos","arcuate line"="arcuate_line",
                                                          "posterior inferior illiac spine"="pos_inf_illiac_sp","posterior superior illiac spine"="pos_sup_illiac_sp","anterior inferior illiac spine"="ant_inf_illiac_sp","anterior superior illiac spine"="ant_sup_illiac_sp","iliac tuberosity"="iliac_tub","auricular surface"="auricular_face","preauricular sulcus"="preauricular_sul","greater sciatic notch"="great_sciatic_not","lesser sciatic notch"="less_Sciatic_not",
                                                          "ishial tuberocity"="ishial_tub","ishial spine"="ishial_sp","ishiopubic ramus"="ishiopubic_ram","iliopubic ramus"="iliopubic_ram","iliopubic eminence"="iliopubic_emin","pubic symphysis"="pubic_symphysis","obturator groove"="obturator_gr","obturator foramen"="obturator_for")))}
  if(bone=="Femur"){out<-tagList(selectInput(paste0("side_",Add),"Side",c("Unknown"="","Right"="_r","Left"="_l")),
                                 selectizeInput(paste0("feature_",Add),"Feature(select all affected)",multiple=TRUE,
                                                choice=c("head","fovae capitis"="fovae_capitis","neck","greater trochanter"="great_trochanter","lesser trochanter"="less_trochanter","intertrochanteric line"="intertrochanteric_line","intertrochanteric crest"="intertrochanteric_cr","trochanteric fossa"="trochanteric_fos","obturator externus groove"="obtutator_externus_gr","gluteal tuberosity/line"="gluteal_tub","pectineal line"="pectineal_line","linea aspera"="linea_aspera",
                                                         "shaft","nutrient foramen"="nutrient_for","spiral line"="spiral_line","medial supracondylar line"="med_supracondylar_line","lateral supracondylar line"="lat_supracondylar_line","popiteal surface"="popiteal_face","lateral condyle"="lat_con","medial condyle"="med_con","popliteal groove"="popliteal_gr","lateral epicondyle"="lat_epicondyle","medial epicondyle"="med_epicondyle","adductor tubercle"="adductor_tubc",
                                                         "intercondylar fossa/notch"="intercondylar_fos","patellar surface"="patellar_face")))}
  if(bone=="Patella"){out<-tagList(selectInput(paste0("side_",Add),"Side",c("Unknown"="","Right"="_r","Left"="_l")),
                                   selectizeInput(paste0("feature_",Add),"Feature(select all affected)",multiple=TRUE,
                                                  choice=c("apex","lateral articular facet"="lat_articular_fac","medial articular facet"="med_articular_fac","anterior surface"="ant_face")))}
  if(bone=="Tibia"){out<-tagList(selectInput(paste0("side_",Add),"Side",c("Unknown"="","Right"="_r","Left"="_l")),
                                 selectizeInput(paste0("feature_",Add),"Feature(select all affected)",multiple=TRUE,
                                                choice=c("tibial plateau"="plateau","medial condyle"="med_con","lateral condyle"="lat_con","intercondylar eminenece"="intercondylar_emin","medial intercondylar tubercle"="med_intercondylar_tubc","lateral intercondylar tubercle"="lat_intercondylar_tubc","superior fibular articular facet"="sup_fibular_fac",
                                                         "tibial tuberocity"="tibial_tub","shaft","popliteal/soleal line"="popliteal_line","nutrient foramen"="nutrient_for","anterior surface"="ant_face","medial surface"="med_face","interosseous crest"="interosseous_cr","interosseous surface"="interosseous_face",
                                                         "medial malleous"="med_malleous","fibular notch"="fibular_not","inferior tibial articular facet"="inf_tibial_fac","malleolar groove"="malleolar_gr","distal articular surface"="dist_articular_face")))}
  if(bone=="Fibula"){out<-tagList(selectInput(paste0("side_",Add),"Side",c("Unknown"="","Right"="_r","Left"="_l")),
                                  selectizeInput(paste0("feature_",Add),"Feature(select all affected)",multiple=TRUE,
                                                 choice=c("head","styloid process"="styloid_pro","proximal articular surface"="prox_articular_face","shaft","interosseous crest"="interosseous_cr","nutrient foramen"="nutrient_for","lateral malleolus"="lat_malleolus","distal articular surface"="dist_articular_face","malleolar fossa"="malleolar_fos","peroneal groove"="peroneal_gr")))}
  if(bone=="Talus"){out<-tagList(selectInput(paste0("side_",Add),"Side",c("Unknown"="","Right"="_r","Left"="_l")),
                                 selectizeInput(paste0("feature_",Add),"Feature(select all affected)",multiple=TRUE,
                                                choice=c("head","body","trochlea","neck","flexor hallucius longus groove"="flexor_hallucius_longus_gr","anterior calcaneal(subtalar) articular surface"="ant_calcaneal_face","middle calcaneal(subtalar) articular surface"="mid_calcaneal_face","posterior calcaneal(subtalar) articular surface"="pos_calcaneal_face","sulcus tali"="tali_sul")))}
  if(bone=="Calcaneous"){out<-tagList(selectInput(paste0("side_",Add),"Side",c("Unknown"="","Right"="_r","Left"="_l")),
                                      selectizeInput(paste0("feature_",Add),"Feature(select all affected)",multiple=TRUE,
                                                     choice=c("calcaneal tuber"="tuber","lateral process"="lat_pro","medial process"="med_pro","sustentaculum tali"="sustentaculum_tali","sustentacular sulcus"="sustentacular_sul","peroneal tubercle"="peroneal_tubc")))}
  if(bone=="Cuniform"){out<-tagList(selectInput(paste0("side_",Add),"Side",c("Unknown"="","Right"="_r","Left"="_l")),
                                    selectInput(paste0("number_",Add),"Cuniform",choice=c("medial(1st)"="_med","intermediate(2nd)"="_inter","lateral(3rd)"="_lat")))}
  if(bone=="snum_Vert"){out<-tagList(h5("nb this section is for the recording of abnormalities on/relating to the supernummery vertebra, the presence of supernummery vertebrae should be recorded in the vertebrae tab."),
                                     selectInput(paste0("bone2_",Add),"Type",c("Cervical","Thorasic","Lumbar","Sacral","cervical/thorasic borderline"="Cervical_Thorasic","thorasic/lumbar borderline"="Thorasic_Lumbar","lumbar/sacral borderline"="Lumbar_Sacral","unknown")),
                                     selectizeInput(paste0("feature_",Add),"Feature(select all affected)",multiple=TRUE,
                                                    choice=c("body","spinous process"="spinous_pro","left pedicle"="pedicle_l","right pedicle"="pedicle_r","vertebral arch right side"="arch_r","vertebral arch left side"="arch_l",
                                                             "right lamina"="lamina_r","left lamina"="lamina_l","right transverse process"="transverse_pro_r","left transverse process"="transvers_pro_l","right superior articular facet"="sup_artical_fac_r","left superior articular facet"="sup_articular_fac_l",
                                                             "right inferior articular facet"="inf_articular_fac_r","left inferior articular facet"="inf_articular_fac_l","right transverse foramen"="transverse_for_r","left transverse foramen"="transverse_for_l",
                                                             "right inferior costal demifacet"="inf_demi_r","left inferior costal demi-facet"="inf_demi_l","right superior costal demi-facet"="sup_demi_r","left superior costal demi-facet"="sup_demi_l","right transverse process costal facet"="transverse_fac_r","left transverse process costal facet"="transverse_fac_l","Right vertebral body facet"="body_fac_r","Left vertebral body facet"="body_fac_l")))}
  if(bone=="snum_Rib"){out<-tagList(h5("nb this section is for the recording of abnormalities on/relating to the supernummery rib, the presence of supernummery ribs should be recorded in the thorax tab."),
                                    selectInput(paste0("bone2_",Add),"Type",c("Cervical","Intrathoracic","Lumbar","Sacral","other/unknown")),
                                    selectInput(paste0("side_",Add),"Side",c("Unknown"="","Right"="_r","Left"="_l")),
                                    selectizeInput(paste0("feature_",Add),"Feature(select all affected)",multiple=TRUE,
                                                   choice=c("head","neck","tubercle","angle","shaft","costal groove"="costal_gr","sternal end"="sternal_end","cranial edge"="cranial_edge","caudal edge"="caudal_edge")))}
  if(bone=="snum_Foot"){out<-tagList(h5("nb this section is for the recording of abnormalities on/relating to the supernummery bone, the presence of supernummery foot bones should be recorded in the Foot tab."),
                                     selectInput(paste0("side_",Add),"Side",c("Unknown"="","Right"="_r","Left"="_l")),
                                     selectInput(paste0("bone2_",Add),"Type",c("other/unknown","os tibiale externum","os trigonum","os peroneum","os intermetatarseum","os subfibulare","os supranaviculare","os subtibiale","os supratalare","os calcaneus secundarius","os vesalianium","os intercuneiforme","os cuboideum secundarium","os tallus accesorius","os tallus secundarius","metatarsophalangeal sesamoid","interphalangeal sesmoid")))}
  if(bone=="snum_Hand"){out<-tagList(h5("nb this section is for the recording of abnormalities on/relating to the supernummery bone, the presence of supernummery hand bones should be recorded in the Hand tab."),
                                     selectInput(paste0("side_",Add),"Side",c("Unknown"="","Right"="_r","Left"="_l")),
                                     selectInput(paste0("bone2_",Add),"Type",c("other/unknown","os centrale","os vesalianum carpi","os gruberi","os radiale externum","os epitrapezium","os epilunatum","os radiostyloideum","os hypolunatum","os hypotriquetrum","os epitriquestum","os triangulare","metacarpalphalangeal sesmoid","interphalangeal sesmoid",
                                                                               "pisiforme secundarium","os hamuli proprium","os hamulare basale","os ulnare externum","os capitatum secundarium","os subcapitatum","os subcapitatum","os styloideum","os parastyloideum","os metastyloideum","os trapezium secundarium","os praetrapezium","os paratrapezium","os trapezoideum secundarium")))}
  if(bone=="snum_Other"){out<-tagList(h5("nb this section is for the recording of abnormalities on/relating to the supernummery bone, the presence of the bone should be recorded in the appropriate tab.Calcified tendons that can be identified should be recorded as a part of the apropreate standard bone"),
                                      selectInput(paste0("side_",Add),"Side",c("Unknown"="","Right"="_r","Left"="_l")),
                                      selectInput(paste0("bone2_",Add),"Type",c("other/unknown","patella cubiti","os supratrochleare dorsale","Medial epicondyle accessory ossicle","os coronoides","bicipital tendon sesamoid","os acromiale","os acetabuli","fabella","cyamella","calcified tendon")))}
  if(bone=="snum_Sutural"){out<-tagList(h5("nb this section is for the recording of abnormalities on/relating to the supernummery bone, the presence of the bone should be recorded in the skull tab.Descriptions of the types of ossicle can also be found in the skull tab"),
                                        selectInput(paste0("side_",Add),"Side",c("Midline/Unknown"="","Right"="_r","Left"="_l")),
                                        selectInput(paste0("bone2_",Add),"Type",c("Inca","Lambda","Lambdoidal","Pterion","Bregma","Coronal","Saggittal")))}
  out
}

lesion1<-function(Add,Type,input){
  out<-br()
  if(Type=="Loss"){out<-tagList(textInput(paste0("size_",Add),"Size of lesion (give atleast 2 dimensions separate with /)"),
                                h5("n.b if made up of multiple foci then give the size of the overall area affected not individual foci"),
                                selectInput(paste0("extent_",Add),"% of bone effected",c("<1/3","1/3-2/3",">2/3")),
                                selectizeInput(paste0("involement_",Add),"Part(s) of bone involved",multiple=TRUE,choice=c("Periosteal/subchondral/external table","Cortex/trabeculae/diploe","Endosteal/internal table")),
                                textInput(paste0("shape_",Add),"Overall shape description"),
                                selectInput(paste0("organisation_",Add),"Organisation",c("Well organised/Focal"="Focal","Irregular/Osteoporotic/Diffuse"="Diffuse")),
                                renderUI(lesion2(Add,Type=input[[paste0("organisation_",Add)]],input)),
                                selectInput(paste0("collapse_",Add),"Associated structural collapse?",c("Yes"=TRUE,"No"=FALSE)))}
  if(Type=="Formation"){out<-tagList(textInput(paste0("size_",Add),"Size of lesion (give atleast 2 dimensions separate with /)"),
                                     h5("n.b if made up of multiple foci then give the size of the overall area affected"),
                                     selectInput(paste0("extent_",Add),"% of bone effected",c("<1/3","1/3-2/3",">2/3")),
                                     textInput(paste0("shape_",Add),"Overall shape description"),
                                     selectInput(paste0("formation_",Add),"Type of formation(select most applicable)",c("Periostal/lamellar reaction"="Periosteal","Spicules formed perpendicular to surface (intact cortex)"="Spicules","Ossified connective tissue"="Connective","Cortex perforation"="Cortex","Endosteal reaction"="Endosteal","Matrix"="Matrix")),
                                     renderUI(lesion2(Add,Type=input[[paste0("formation_",Add)]],input)),
                                     renderUI(lesion2(Add,Type=input[[paste0("type2_",Add)]],input))
  )}
  if(Type=="Complex"){out<-tagList(textInput(paste0("size_",Add),"Size of lesion (give atleast 2 dimensions separate with /)"),
                                   h5("n.b if made up of multiple foci then give the size of the overall area affected not individual foci"),
                                   selectInput(paste0("extent_",Add),"% of bone effected",c("<1/3","1/3-2/3",">2/3")),
                                   selectizeInput(paste0("involement_",Add),"Part(s) of bone involved",multiple=TRUE,choice=c("Periosteal/subchondral/external table","Cortex/trabeculae/diploe","Endosteal/internal table")),
                                   h4("Formation"),
                                   selectInput(paste0("formation_",Add),"Type of formation(select most applicable)",c("Periostal/lamellar reaction"="Periosteal","Spicules formed perpendicular to surface (intact cortex)"="Spicules","Ossified connective tissue"="Connective","Cortex perforation"="Cortex","Endosteal reaction"="Endosteal","Matrix"="Matrix")),
                                   renderUI(lesion2(Add,Type=input[[paste0("formation_",Add)]],input)),
                                   renderUI(lesion2(Add,Type=input[[paste0("type2_",Add)]],input)),
                                   h4("Loss"),
                                   selectInput(paste0("organisation_",Add),"Type of bone loss",c("Well organised/Focal"="Focal","Irregular/Osteoporotic/Diffuse"="Diffuse")),
                                   renderUI(lesion2(Add,Type=input[[paste0("organisation_",Add)]],input)),
                                   selectInput(paste0("collapse_",Add),"Associated structural collapse?",c("Yes"=TRUE,"No"=FALSE)),
                                   h4("Connection between loss and formation"),
                                   selectInput(paste0("connection_",Add),"Type of connection",c("healing (single stage of process)","healing(multi-stage,healing and active portions)","overlapping lesions","unknown")),
                                   textInput(paste0("shape_",Add),"Overall shape description"))}
  if(Type=="Shape"){out<-tagList(selectInput(paste0("extent_",Add),"Degree of abnormality",c("Barely discernable","Clearly discernable")),
                                 selectInput(paste0("shape_",Add),"Type of abnormality",c("Reduced size","Increased size","Bowing/Abnormal curvature","Angulation","Premature fusion","Failure of normal fusion","Abnormal width","Other")),
                                 renderUI(lesion2(Add,Type=input[[paste0("shape_",Add)]],input,bone=input[[paste0("bone_",Add)]])))}
  out
}

lesion2<-function(Add,Type,input,bone=NA){
  out<-br()
  if(Type=="Focal"){out<-tagList(selectInput(paste0("sites_",Add),"Number of Foci",c("Unifocal"="1","2 foci"="2","3-5 foci"="3-5","6-10 foci"="6-10","10+ foci"="10")),
                                 selectizeInput(paste0("size2_",Add),"Foci size(select all applicable)",multiple=TRUE,choice=c("<1cm","1-5cm",">5cm")),
                                 selectizeInput(paste0("response_",Add),"Bony response",choice=c("Circumscription/sclerotic reaction","Boundaries Well defined but no sclerosis","Margins not sharply defined")))}
  if(Type =="Diffuse"){out<-tagList(selectInput(paste0("thining_",Add),"Cortical thinning?",c("Yes"=TRUE,"No"=FALSE)),
                                    selectInput(paste0("sites_",Add),"Number of separate sites",c("1","2","3-5","6-10","10+")),
                                    h5("nb sites can vary in size and extent,record the number of distinct sites of resorption"),
                                    selectInput(paste0("overlapping_",Add),"Overlapping(select the best description)",c("One irregular site with no deserable separation"="single","multiple irregular sites with no overlap"="separate","Overlapping sites with original separation still visible"="overlapping","Mixture of overlapping and separate sites"="mixed"))
  )}
  if(Type=="Periosteal"){out<-selectInput(paste0("type2_",Add),"Type of reaction",c("reactive woven bone"="Woven","sclerotic reaction"="Sclerotic","Mixture of sclerotic and woven"="Mixed"))}
  if(Type=="Spicules"){out<-selectInput(paste0("type2_",Add),"Pattern",c("Sunburst","Cauliflower","Other"))}
  if(Type=="Connective"){out<-selectInput(paste0("type2_",Add),"Type",c("Myostis ossificans (without fracture)"="Myostis Ossificans","Myostis ossificans (with fracture)"="Myostis Ossificans_fracture","Enthesopathy","Joint fussion"))}
  if(Type=="Cortex"){out<-selectInput(paste0("type2_",Add),"cause of perforation",c("Expansion shell type reaction","Cloacae/sinus tracks","Other"))}
  if(Type=="Endosteal"){out<-selectInput(paste0("type2_",Add),"Lamellae visibility",c("Lamellae visible","Medullary cavity narrowed no visible lamellae"))}
  if(Type=="Matrix"){out<-selectInput(paste0("type2_",Add),"Type of abnormal matrix",c("Deposition of woven bone","Cancellous expansion","Trabecular coarsening"))}
  if(Type=="Other"){out<-textInput(paste0("other_",Add),"Define other")}
  if(Type=="Joint fussion"){out<-selectInput(paste0("joint_",Add),"Extent of fusion",c("Single small/narrow bony connection across the joint"="single small","2-3 small separate connections"="2-3 small",">3 small connections"=">3 small","Single large connection (not covering entire the joint)"="single large","Multiple large connections"="Multiple large","Multiple connections of various sizes"="Multiple various","Complete fusion/ very little or no gaps"="Complete"))}
  if(Type=="Enthesopathy"){out<-textInput(paste0("Enthesopathy_",Add),"Tendon/Ligament name",value="Unknown")}
  if(Type=="Myostis Ossificans_fracture"){out<-selectizeInput(paste0("fracture_",Add),"Type of fracture",multiple=TRUE,choice=c("Complete","Partial","Greenstick","Simple","Comminuted","Spiral","Compression","Depressed","Pathological","Unknown"))}
  if(Type=="Expansion shell type reaction"){out<-selectizeInput(paste0("shell_",Add),"Type shell(select all applicable)",multiple=TRUE,choice=c("continuous","interrupted","expanded cortex","lobulated","ridged/trabeculated soap bubble","single","lamellated","lamellated onion skin","butress","common angle","spiculated","parallel spiculated/hair on end","spiculated sunburst"))}
  if(Type=="Cloacae/sinus tracks"){out<-numericInput(paste0("cloacae_",Add),"Number of cloacae",value=NA)}
  if(Type=="Reduced size"){out<-tagList(selectInput(paste0("local_",Add),"Localised?",c("Yes,this is the only bone affected"="yes_only","yes,only a small number of articulated bones are effected"="yes_regional","no,multiple bones acrosss the skeleton are of reduced size"="no")),
                                        selectInput(paste0("proportional_",Add),"Shape in relation to a 'normal' sized example",c("The bones features are all present and in proportion"="proportional reduction","The bones features are all present but not proportional"="non-proportional reduction","There are features missing/ significantly destored"="destored shape reduction","unknown/undeterminable")),
                                        textInput(paste0("proportional2_",Add),"further description of relative shape",value="none"),
                                        textInput(paste0("size_",Add),"Measurements(mm)",value="1,2"),
                                        textInput(paste0("size2_",Add),"Measurments given",value="Measurment1,Measurment2"),
                                        h5("nb the measurements and measurments given should be in the same order with multiple entries separated with a ,.You should include enough measurments to give a reliable impression of the abnormality"))}
  if(Type=="Increased size"){out<-tagList(selectInput(paste0("local_",Add),"Localised?",c("Yes,this is the only bone affected"="yes_only","yes,a small number of articulated bones are effected"="yes_regional","no,multiple bones acrosss the skeleton are of increased size"="no")),
                                          selectInput(paste0("proportional_",Add),"Shape in relation to a 'normal' sized example",c("The bones features are all present and in proportion"="proportional increase","The bones features are all present but not proportional"="non-proportional increase","There are features missing/ significantly destored"="destored shape increase","unknown/undeterminable")),
                                          textInput(paste0("proportional2_",Add),"further description of relative shape",value="none"),
                                          textInput(paste0("size_",Add),"Measurements(mm)",value="1,2"),
                                          textInput(paste0("size2_",Add),"Measurments given",value="Measurment1,measurment2"),
                                          h5("nb the measurements and measurments given should be in the same order with multiple entries separated with a ,.You should include enough measurments to give a reliable impression of the abnormality"))}
  if(Type=="Abnormal width"){
    if(bone %in% c("Femur","Tibia","Fibula","Phalanx","Humerus","Radius","Ulna","Metacarpal","Metatarsal")){out<-tagList(selectInput(paste0("form_",Add),"Form",c("Flaring proximal metaphysis","Flaring distal metaphysis","Flaring of both metaphyses","Uniform abnormal width","Spindle shaped","Other")),
                                                                                                                         h4("Measurments"),
                                                                                                                         h5("you should record enough of the measurments below to give a reliable impression of the abnormality"),
                                                                                                                         numericInput(paste0("width1_",Add),"mid shaft diameter",value=NA),
                                                                                                                         numericInput(paste0("width2_",Add),"distal shaft diameter",value=NA),
                                                                                                                         numericInput(paste0("width3_",Add),"proximal shaft diameter",value=NA),
                                                                                                                         numericInput(paste0("width4_",Add),"max shaft diamter",value=NA),
                                                                                                                         numericInput(paste0("width5_",Add),"min shaft diameter",value=NA),
                                                                                                                         numericInput(paste0("width6_",Add),"distal metaphysis width",value=NA),
                                                                                                                         numericInput(paste0("width7_",Add),"proximal metaphysis width",value=NA))}
    else{out<-tagList(selectInput(paste0("form_",Add),"Form",c("uniformally wide","uniformally narrow","centeral widening","centeral narrowing","peripheral widening","peripheral narrowing","Other/Unknown")),
                      textInput(paste0("size_",Add),"Widths(mm)",value="1,2"),
                      textInput(paste0("size2_",Add),"Measurments given",value="Measurment1,measurment2"),
                      h5("nb the 'Widths' and 'Measurments given' should be in the same order with multiple entries separated with a ,.You should include enough measurments to give a reliable impression of the abnormality."))}
  }
  if(Type=="Bowing/Abnormal curvature"){
    if(bone %in% c("Atlas","Axis","Cervical","Thoracic","Lumbar","Sacrum")){out<-tagList(selectInput(paste0("curve_",Add),"Type",c("Kyphosis","Scoliosis,left","Scoliosis,right")),
                                                                                         selectInput(paste0("form_",Add),"Form",c("Angular","Gradual change in body height")),
                                                                                         selectInput(paste0("ankylosis_",Add),"Ankylosis present",c("yes"=TRUE,"no"=FALSE)))}
    else{out<-tagList(selectInput(paste0("direction_",Add),"Direction of bowing",c("Medial","Lateral","Anterior","Posterior","Unknown")),
                      selectInput(paste0("extent2_",Add),"% of bone effected",c("<1/3","1/3-2/3",">2/3")),
                      numericInput(paste0("length1_",Add),"length of long axis(mm)",value=NA),
                      numericInput(paste0("length2_",Add),"anatomical length(mm)",value=NA),
                      h5("nb the anatomical length is the shortest distance from most superior to the most inferior points, in an unbowed bone this would be approximate to the length of the long axis"))}
  }
  if(Type=="Angulation"){out<-tagList(selectInput(paste0("direction_",Add),"Direction of angulation",c("Medial","Lateral","anterior","posterior","superior","inferior")),
                                      selectInput(paste0("angle_",Add),"Degree of angulation",c(">170","150-170","125-150","100-125","<100")),
                                      h5("nb the degree of angulation refers to the smallest angle between the long axis of the bone and the angulated portion. Therefore 180 would indicate no angulation (straight)"),
                                      selectInput(paste0("fracture_",Add),"relate to healed fracture?",c("Yes","No","Unknown")))}
  if(Type=="Premature fusion"){}
  if(Type=="Failure of normal fusion"){}
  out
}

Custom_RC<-function(input,Add){# produces PP table from custom UI
  
  extract<-function(x,n=Add,y=input){
    y[[as.character(paste0(x,n))]]
  }
  
  #set up table
  Table<-data.frame(ID=NA,In=NA,D=NA,ID2=paste0("C_",Add),Type=NA,Des=NA,Loc=NA,Feat=NA,Size=NA,Shape=NA,Nature=NA,Add=NA,Link=NA)
  
  #fill in straight fields (same for all options)
  Table$Type<-extract("type_")
  Table$Des<-"Custom lesion"
  if(extract("link1_") != "None"){Table$Link<-paste(paste(extract("link1_"),collapse=","),paste(extract("link2_"),collapse=","),sep=":")}else{Table$Link<-NA}
  Table$Add<-extract("appearance_")
  
  #Location (Table$Loc,Table$Feat)
  region<-extract("region_")
  Unpaired<-c("Frontal","Occipital","Mandible","Ethmoid","Vomer","Hyoid","Sternum","Sacrum","Coccyx")
  featureless<-c("Scaphoid","Hamate","Capitate","Lunate","Pisiform","Triquetral","Trapezium","Trapezoid","Cuboid","Navicular")
  
  if(region=="Skull"){
    if(extract("bone_")%in%Unpaired){bone<-extract("bone_")}else{bone<-paste0(extract("bone_"),extract("side_"))}
    if(bone%in% c("Hyoid","Mandible")){
      Table$Loc<-paste(region,bone,extract("location_"),sep=":")
      Table$Feat<-paste(paste(extract("feature_"),collapse=","),NA,sep=":")}
    if(extract("bone_")%in% c("Cochnea","Vomer","Lacriminal","Nasal")){
      Table$Feat<-paste(NA,paste(extract("suture_",collapse=","),sep=":"))
      if(bone=="Vomer"){Table$Loc<-paste0("SKull:Vomer:",extract("location_"))
      }else{Table$Loc<-paste(region,bone,paste(extract("surface_"),extract("location_"),sep=","),sep=":")}}
    if(extract("bone_")%in% c("Palatine","Sphenoid","Ethmoid")){
      Table$Loc<-paste(region,bone,extract("location_"),sep=":")
      Table$Feat<-paste(paste(extract("feature_"),collapse=","),paste(extract("suture_"),collapse=","),sep=":")}
    if(extract("bone_")%in% c("Frontal","Parietal","Occipital","Zygomatic","Temporal","Maxilla")){
      Table$Loc<-paste(region,bone,paste(extract("surface_"),extract("location_"),sep=","),sep=":")
      Table$Feat<-paste(paste(extract("feature_"),collapse=","),paste(extract("suture_"),collapse=","),sep=":")}
  }
  
  if(region=="Vertebrae"){
    Table$Feat<-paste(paste(extract("feature_"),collapse=","),NA,sep=":")
    if(extract("bone_")%in%c("Atlas","Axis","Sacrum")){Table$Loc<-paste("Vertebrae",extract("bone_"),extract("location_"),sep=":")}
    if(extract("bone_")%in%c("Lumbar","Cervical","Thoracic")){Table$Loc<-paste("Vertebrae",paste(extract("bones_"),collapse=","),extract("location_"),sep=":")}
  }
  
  if(region%in%c("Pelvis","Shoulder","Leg","Arm","Thorax")){
    if(extract("bone_")%in%Unpaired){bone<-extract("bone_")}else{bone<-paste0(extract("bone_"),extract("side_"))}
    Table$Loc<-paste(region,bone,extract("location_"),sep=":")
    Table$Feat<-paste(paste(extract("feature_"),collapse=","),NA,sep=":")
    if(bone=="Coccyx"){Table$Feat<-"NA:NA"}
  }
  
  if(region%in%c("Hand","Foot")){
    if(extract("bone_")%in% c("Phalanx","Metacarpal","Metatarsal","Cuniform")){
      bone<-paste0(extract("bone_"),extract("number_"),extract("side_"))
      Table$Loc<-paste(region,bone,extract("location_"),sep=":")
      Table$Feat<-paste(paste(extract("feature_"),collapse=","),NA,sep=":")
      if(extract("bone_")=="Cuniform"){Table$Feat<-"NA:NA"}}
    if(extract("bone_")%in% c("Scaphoid","Hamate","Capitate","Lunate","Pisiform","Triquetral","Trapezium","Trapezoid","Cuboid","Navicular")){
      bone<-paste0(extract("bone_"),extract("side_"))
      Table$Loc<-paste(region,bone,extract("location_"),sep=":")
      Table$Feat<-"NA:NA"}
    if(extract("bone_")%in%c("Talus","Calcaneous")){
      bone<-paste0(extract("bone_"),extract("side_"))
      Table$Loc<-paste(region,bone,extract("location_"),sep=":")
      Table$Feat<-paste(paste(extract("feature_"),collapse=","),NA,sep=":")}
  }
  
  if(region=="Other"){
    bone<-extract("bone_")
    if(bone=="snum_Vert"){
      Table$Loc<-paste0("Vertebrae:Snum:",paste(extract("bone2_"),extract("location_"),sep=","))
      Table$Feat<-paste(paste(extract("feature_"),collapse=","),NA,sep=":")}
    if(bone=="snum_Rib"){
      bone2<-paste0(extract("bone2_"),extract("side_"))
      Table$Loc<-paste0("Thorax:Snum:",paste(bone2,extract("location_"),sep=","))
      Table$Feat<-paste(paste(extract("feature_"),collapse=","),NA,sep=":")}
    if(bone=="snum_Sutural"){
      Sutures<-data.frame(bone=c("Inca","Lambda","Lambdoidal","Pterion","Bregma","Coronal","Saggittal"),
                          suture=c("lambdoid","lambdoid","lambdoid","sphenoparietal","coronal","coronal","saggital"))
      bone2<-paste0(extract("bone2_"),extract("side_"))
      Table$Loc<-paste0("Skull:Snum:",paste(bone2,extract("location_"),sep=","))
      Table$Feat<-paste0("NA:",Sutures$suture[Sutures$bone==extract("bone2_")])}
    if(bone=="snum_Hand"){
      bone2<-paste0(extract("bone2_"),extract("side_"))
      Table$Loc<-paste0("Hand:Snum:",paste(bone2,extract("location_"),sep=","))
      Table$Feat<-"NA:NA"}
    if(bone=="snum_Foot"){
      bone2<-paste0(extract("bone2_"),extract("side_"))
      Table$Loc<-paste0("Foot:Snum:",paste(bone2,extract("location_"),sep=","))
      Table$Feat<-"NA:NA"}
    if(bone=="snum_Other"){
      regions<-data.frame(bone=c("other/unknown","patella cubiti","os supratrochleare dorsale","Medial epicondyle accessory ossicle","os coronoides","bicipital tendon sesamoid","os acromiale","os acetabuli","fabella","cyamella","calcified tendon"),
                          region=c(NA,"Leg","Arm","Arm","Arm","Arm","Shoulder","Pelvis","Leg","Leg",NA))
      bone2<-paste0(extract("bone2_"),extract("side_"))
      Table$Loc<-paste(regions$region[regions$bone==extract("bone2_")],bone2,extract("location_"),sep=":")
      Table$Feat<-"NA:NA"}
  }
  
  #lesion description (Table$Size,Table$Shape,Table$Nature)
  
  if(extract("type_")=="Loss"){
    Table$Shape<-paste0("Overall:",extract("shape_"))
    if(extract("organisation_")=="Focal"){
      Table$Size<-paste0("Total Area,% bone,no. Foci,Foci size:",paste(extract("size_"),extract("extent_"),extract("sites_"),paste(extract("size2_"),collapse="/"),sep=","))
      Table$Nature<-paste("Organisation,Involvement,Response,collapse:Focal",paste(extract("involement_"),collapse="/"),extract("response_"),extract("collapse_"),sep=",")
    }
    if(extract("organisation_")=="Diffuse"){
      Table$Size<-paste0("Total Area,% bone,no. sites:",paste(extract("size_"),extract("extent_"),extract("sites_"),sep=","))
      Table$Nature<-paste("Organisation,Involvement,cortical thinning,overlapping,collapse:Diffuse",paste(extract("involement_"),collapse="/"),extract("thining_"),extract("overlapping_"),extract("collapse_"),sep=",")
    }
  }
  
  if(extract("type_")=="Formation"){
    Table$Size<-paste0("Total area,% bone:",paste(extract("size_"),extract("extent_"),sep=","))
    Table$Shape<-paste0("Overall:",extract("shape_"))
    if(extract("formation_")=="Periosteal"){Table$Nature<-paste("Type,Reaction:Periosteal",extract("type2_"),sep=",")}
    if(extract("formation_")=="Spicules"){
      if(extract("type2_")=="Other"){Table$Nature<-paste("Type,Pattern:Spicules",extract("other_"),sep=",")}
      else{Table$Nature<-paste("Type,Pattern:Spicules",extract("type2_"),sep=",")}}
    if(extract("formation_")=="Connective"){
      if(extract("type2_")=="Myostis Ossificans"){Table$Nature<-"Type,Connective type,Fracture:Connective,Myostosis Ossificans,FALSE"}
      if(extract("type2_")=="Myostis Ossificans_fracture"){Table$Nature<-paste0("Type,Connective type,Fracture:Connective,Myostosis Ossificans,",paste(extract("fracture_"),collapse="/"))}
      if(extract("type2_")=="Enthesopathy"){Table$Nature<-paste0("Type,Connective type,Tendon/ligament:Connective,Enthesopathy,",extract("Enthesopathy_"))}
      if(extract("type2_")=="Joint fussion"){Table$Nature<-paste0("Type,Connective type,extent:Connective,Joint Fussion,",extract("joint_"))}}
    if(extract("formation_")=="Cortex"){
      if(extract("type2_")=="Expansion shell type reaction"){Table$Nature<-paste0("Type,Cause,Shell:Cortex Perforation,Expansion shell reaction,",paste(extract("shell"),collapse="/"))}
      if(extract("type2_")=="Cloacae/sinus tracks"){Table$Nature<-"Type,Cause:Cortex Perforation,Cloacae/Sinus tracks";Table$Size<-paste0("Total area,% bone,no.Cloacae:",paste(extract("size_"),extract("extent_"),extract("cloacae_"),sep=","))}
      if(extract("type2_")=="Other"){Table$Nature<-paste0("Type,Cause:Cortex Perforation,",extract("other_"))}}
    if(extract("formation_")=="Endosteal"){Table$Nature<-paste0("Type,Lamellae visibility:Endosteal,",extract("type2_"))}
    if(extract("formation_")=="Matrix"){Table$Nature<-paste0("Type,Matrix type:Matrix,",extract("type2_"))}
  }
  
  if(extract("type_")=="Complex"){
    Table$Shape<-paste0("Overall:",extract("shape_"))
    
    Size_Flab<-"Total area,% bone";Size_Fdata<-paste(extract("size_"),extract("extent_"),sep=",")
    if(extract("formation_")=="Periosteal"){Nature_Flab<-"Type,Reaction";Nature_Fdata<-paste0("Periosteal,",extract("type2_"))}
    if(extract("formation_")=="Spicules"){
      if(extract("type2_")=="Other"){Nature_Flab<-"Type,Pattern";Nature_Fdata<-paste0("Spicules,",extract("other_"))}
      else{Nature_Flab<-"Type,Pattern";Nature_Fdata<-paste0("Spicules,",extract("type2_"))}}
    if(extract("formation_")=="Connective"){
      if(extract("type2_")=="Myostosis Ossificans"){Nature_Flab<-"Type,Connective type,Fracture";Nature_Fdata<-"Connective,Myostosis Ossificans,FALSE"}
      if(extract("type2_")=="Myostosis Ossificans_fracture"){Nature_Flab<-"Type,Connective type,Fracture";Nature_Fdata<-paste0("Connective,Myostosis Ossificans,",paste(extract("fracture_"),collapse="/"))}
      if(extract("type2_")=="Enthesopathy"){Nature_Flab<-"Type,Connective type,Tendon/ligament";Nature_Fdata<-paste0("Connective,Enthesopathy,",extract("Enthesopathy_"))}
      if(extract("type2_")=="Joint fussion"){Nature_Flab<-"Type,Connective type,extent";Nature_Fdata<-paste0("Connective,Joint Fussion,",extract("joint_"))}}
    if(extract("formation_")=="Cortex"){
      if(extract("type2_")=="Expansion shell type reaction"){Nature_Flab<-"Type,Cause,Shell";Nature_Fdata<-paste0("Cortex Perforation,Expansion shell reaction,",paste(extract("shell"),collapse="/"))}
      if(extract("type2_")=="Cloacae/sinus tracks"){Nature_Flab<-"Type,Cause";Nature_Fdata<-"Cortex Perforation,Cloacae/Sinus tracks"
      Size_Flab<-"Total area,% bone,no.Cloacae";Size_Fdata<-paste(extract("size_"),extract("extent_"),extract("cloacae_"),sep=",")}
      if(extract("type2_")=="Other"){Nature_Flab<-"Type,Cause";Nature_Fdata<-paste0("Cortex Perforation,",extract("other_"))}}
    if(extract("formation_")=="Endosteal"){Nature_Flab<-"Type,Lamellae visibility";Nature_Fdata<-paste0("Endosteal,",extract("type2_"))}
    if(extract("formation_")=="Matrix"){Nature_Flab<-"Type,Matrix type";Nature_Fdata<-paste0("Matrix,",extract("type2_"))}
    
    if(extract("organisation_")=="Focal"){
      Size_Llab<-"no. Foci,Foci size";Size_Ldata<-paste(extract("sites_"),paste(extract("size2_"),collapse="/"),sep=",")
      Nature_Llab<-"Organisation,Involvement,Response,collapse";Nature_Ldata<-paste("Focal",paste(extract("involement_"),collapse="/"),extract("response_"),extract("collapse_"),sep=",")}
    if(extract("organisation_")=="Diffuse"){
      Size_Llab<-"no. sites";Size_Ldata<-extract("sites_")
      Nature_Llab<-"Organisation,Involvement,cortical thinning,overlapping,collapse";Nature_Ldata<-paste("Diffuse",paste(extract("involement_"),collapse="/"),extract("thining_"),extract("overlapping_"),extract("collapse_"),sep=",")}
    
    Table$Size<-paste(paste(Size_Flab,Size_Llab,sep=","),paste(Size_Fdata,Size_Ldata,sep=","),sep=":")
    Table$Nature<-paste(paste(Nature_Flab,Nature_Llab,"Connection",sep=","),paste(Nature_Fdata,Nature_Ldata,extract("connection_"),sep=","),sep=":")
  }
  if(extract("type_")=="Shape"){
    if(extract("shape_")%in% c("Reduced size","Increased size")){
      Table$Size<-paste(extract("size2_"),extract("size_"),sep=":")
      Table$Shape<-paste("Overall,Proportional,Add:",paste(extract("shape_"),extract("proportional_"),extract("proportional2_"),sep=","))
      Table$Nature<-paste0("Localised:",extract("local_"))}
    if(extract("shape_")=="Bowing/Abnormal curvature"){
      if(extract("bone_")%in%c("Axis","Atlas","Cervical","Thoracic","Lumbar","Sacrum")){
        Table$Nature<-paste0("Ankylosis:",extract("ankylosis_"))
        Table$Shape<-paste("Overall,Curvature,form:Bowing/Abnormal curvature",extract("curvature_"),extract("form_"),sep=",")
        Table$Size<-"NA:NA"
      }else{
        Table$Nature<-"NA:NA"
        Table$Shape<-paste("Overall,Direction:Bowing/Abnormal curvature",extract("direction_"))
        Table$Size<-paste0("% bone,Long axis,Anatomical length:",paste(extract("extent2_"),extract("length1_"),extract("lengh2_"),sep=","))}}
    if(extract("shape_")=="Angulation"){
      Table$Nature<-"NA:NA"
      Table$Shape<-paste("Overall,Direction:Angulation",extract("direction_"))
      Table$Size<-paste0("Degree of angulation:",extract("angle_"))}
    if(extract("shape_")=="Premature fusion"){}
    if(extract("shape_")=="Failure of normal fusion"){}
    if(extract("shape_")=="Abnormal Width"){
      Table$Shape<-paste0("Overall,Form:Abnormal width,",extract("form_"))
      Table$Nature<-"NA:NA"
      if(extract("bone_")%in%c("Femur","Tibia","Fibula","Phalanx","Humerus","Radius","Ulna","Metacarpal","Metatarsal")){
        Table$Size<-paste0("Mid-shaft,Distal shaft,Proximal shaft,Max shaft,Min shaft,distal metaphysis,proximal metaphysis:",paste(extract("width1_"),extract("width2_"),extract("width3_"),extract("width4_"),extract("width5_"),extract("width6_"),extract("width7_"),sep=","))
      }else{
        Table$Size<-paste(extract("size2_"),extract("size_"),sep=":")}}
    if(extract("shape_")=="Other"){
      Table$Shape<-"Overall:Other"
      Table$Nature<-paste0("Other description:",extract("other_"))
      Table$Size<-"NA:NA"}
  }
  
  Table
}