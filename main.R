# Inputs 
params = list()
params$user_id = "tintin"
# Identifier la recherche grâce à des mots clés (Intitulé du poste + type de contrat + zone)
params$mots_cles = c("data+scientist", "Statisticien")
# Functions
source("fnct.R")
# Scraper une liste d'offres
get(mc = params$mots_cles, id = params$user_id)
# Classer manuelement (swap) les offres
app(id = params$user_id)
# Apprendre du résultat et prédir la classification des non classés
algo(id = params$user_id)
# Proposer les offres les plus succeptible de plaire
read_rds(paste0("data/get/",params$user_id,"/top/prev.RDS")) %>% 
  slice_head(n = 10) %>% 
  View()