# MafTools_function_MO #
# Part 1 - VCF2MAF ####
# Attach packages: 
lapply(c("vcfR", "maftools", "anor", "BioInstaller"),
       require, character.only = TRUE)
# initialise:
annovar.dir <- "/home/olanipekunm/R/x86_64-pc-linux-gnu-library/4.1/annovar"
database.dir <- "/home/olanipekunm/R/x86_64-pc-linux-gnu-library/4.1/annovar/humandb"
vcfanno.dir <- "/home/olanipekunm/R/x86_64-pc-linux-gnu-library/4.1/vcfanno.dir"
# install.bioinfo('annovar', annovar.dir)
# install.bioinfo('vcfanno', vcfanno.dir)
# download.database('db_annovar_refgene', database.dir = database.dir, buildver = "hg19")
# download.database('db_ucsc_cytoband', database.dir = database.dir, buildver = "hg19")
# download.database('db_annovar_avsnp147', database.dir = database.dir, buildver = "hg19") #did not work
# download.database('db_annovar_refgene', database.dir = database.dir, buildver = "hg38")
# download.database('db_ucsc_cytoband', database.dir = database.dir, buildver = "hg38")
# download.database('db_annovar_avsnp147', database.dir = database.dir, buildver = "hg38") #did not work

# Colour palettes
colorme <- c('#FDAE61', '#80B1D3', '#FB8072', '#BEBADA', '#FDE0EF', 
             '#B3E2CD', '#FDCDAC', '#80CDC1', '#FFF2AE','#AA9486',
             '#82B886', '#E9BF7C', '#E6637A', '#8C6EB8', '#628BEB', 
             '#838536', '#EBA29C', '#99A0B6', '#B8887B','#B5A991' ,
             '#61B8A5', '#366F85', '#F09870')

# vcf2maf functions - WIP
VCF2VCFanno <- function(indir, outdir, indir.p = NULL, outdir.p = NULL, db){
  # For converting from vcf to maf and then reading in the maf with annovar and maftools
  # indir = input file path (directory) containing all .vcf - caveman
  # outdir = output file path directory - caveman
  # indir.p = input file path (directory) containing all .vcf - pindel
  # outdir.p = output file path directory - pindel
  # db = 'hg19', 'hg20' or 'hg38' 
  
  # Caveman part
  caveman.files <- list.files(indir, pattern = "\\.gz$", full.names = TRUE)
  cave.anno.vcf <- list()
    cave.anno.vcf <- sapply(caveman.files, function(x){
      try(anor::annotation(anno.name = "perl_annovar_refGene", buildver = db,
                 annovar.dir = annovar.dir, database.dir = database.dir, 
                 input.file = x,
                 out = paste0(outdir, basename(x), ".vep"),
                 vcfinput = TRUE), silent = TRUE) 
    })
  # Pindel part
  pin.anno.vcf <- list()
  pin.maf <- list()
  if (!is.null(indir.p)) {
    pindel.files <- list.files(indir.p, pattern = "\\.gz$", full.names = TRUE)
    pin.anno.vcf <- sapply(pindel.files, function(x){
      try(anor::annotation(anno.name = "perl_annovar_refGene", buildver = db,
                 annovar.dir = annovar.dir, database.dir = database.dir,
                 input.file = x,
                 out = paste0(outdir.p, basename(x), ".vep"),
                 vcfinput = TRUE), silent = TRUE)
    })
  }
}
ReadTheMAFs <- function(mafdir.c, mafdir.p = NULL) {
  # For when the mafs have already been converted and must only be read in
  # Reading caveman MAFs
  cave.maf <- lapply(list.files(mafdir.c, full.names = TRUE)[grep("*.hg38_multianno.txt", list.files(mafdir.c))], function(file1) {
    try(read.maf(maf = annovarToMaf(file1, refBuild = "hg38")), silent = TRUE)
  })
  cave.maf <- cave.maf[sapply(cave.maf, class) != "try-error"]
  
  # Reading pindel MAFs
  if (!is.null(mafdir.p)){
    pin.maf <- lapply(list.files(mafdir.p, full.names = TRUE)[grep("*.hg38_multianno.txt", list.files(mafdir.p))], function(file2) {
      try(read.maf(maf = annovarToMaf(file2, refBuild = "hg38")), silent = TRUE)
    })
    pin.maf <- pin.maf[sapply(pin.maf, class) != "try-error"]
  }

  # Combine the MAFs
  if (!is.null(pin.maf)) {
    cave.pin.maf <- c(cave.maf, pin.maf)
  } else {
    cave.pin.maf <- cave.maf
  }
  
  # Merge the MAFs
  CPmaf <- merge_mafs(cave.pin.maf)
  
  return(CPmaf)
}
VCF2MAF2Read <- function(indir, outdir, indir.p = NULL, outdir.p = NULL, db){
  VCF2VCFanno(indir, outdir, indir.p = NULL, outdir.p = NULL, db)
  ReadTheMAFs(mafdir.c = outdir, mafdir.p = outdir.p)
}

# Metadata2clin ####
caveman.files <- list.files(indir) # use path as before for indir input
list_ids <- unique(str_extract(caveman.files[str_detect(caveman.files, "caveman")], "PD[0-9]{5}a"))
metadata <- read.csv("/data/mutographs/files/Somatic/RCC_Balkan/metadata/Balkan_RCC_meta_data/MutWP1_Balkan_RCC_core_data_Train21.csv") %>%
  filter(donor_id %in% list_ids) %>% arrange(., donor_id)
clin <- metadata %>% dplyr::select(donor_id, sex, age_diag, country, tobacco, alcohol, histo_type)
names(clin)[1] <- "Tumor_Sample_Barcode"
clin$Tumor_Sample_Barcode <- as.factor(clin$Tumor_Sample_Barcode)
CPmaf@clinical.data <- data.table(clin)

# Part 2 - MAFTOOLS ####
resdir <- "/home/olanipekunm/R/Balkans/Balkans_Genomics_MO/Results/" # path to directory for results
# 1. Oncoplot
vc_cols <- colorme[1:8]
names(vc_cols) = c(
  'Frame_Shift_Del','Missense_Mutation','Nonsense_Mutation',
  'Multi_Hit','Frame_Shift_Ins','In_Frame_Ins',
  'Splice_Site','In_Frame_Del')
print(vc_cols)

pdf(paste0(resdir,"RCC/Maftools_blk_rcc_oncoplot.pdf"), width = 6, height = 4)
oncoplot(CPmaf, top = 10, colors = vc_cols, bgCol = "white")
dev.off()

# annotation:
fabcolors = RColorBrewer::brewer.pal(n = 2,name = 'Spectral')
names(fabcolors) = c("Female", "Male")
fabcolors = list(sex = fabcolors)
oncoplot(CPmaf, top = 10, colors = vc_cols, bgCol = "white", clinicalFeatures = 'sex', annotationColor = fabcolors)
clin$Tumor_Sample_Barcode <- as.factor(clin$Tumor_Sample_Barcode)
CPmaf@clinical.data <- data.table(clin)

# 2. TiTv
pdf(paste0(resdir,"RCC/Maftools_blk_rcc_titv.pdf"), width = 6, height = 4)
titv(maf = CPmaf, plot = TRUE, useSyn = F)
dev.off()

# 3. Rainfall
for (i in 1:length(list_ids)){
  rainfallPlot(maf = CPmaf, detectChangePoints = T, ref.build = "hg38", tsb = list_ids[i])
}

pdf(paste0(resdir,"RCC/Maftools_blk_rcc_rainfall_PD59937a.pdf"), width = 6, height = 4)
rainfallPlot(maf = CPmaf, detectChangePoints = T, ref.build = "hg38")
dev.off()

# Oncodrive
#z-score
onc.sig <- oncodrive(maf = CPmaf, AACol = "aaChange", minMut = 5, pvalMethod = 'zscore')
head(onc.sig)
plotOncodrive(res = onc.sig, fdrCutOff = 0.1, useFraction = TRUE, labelSize = 0.5)
# poisson
onc.sig.1 <- oncodrive(maf = CPmaf, AACol = "aaChange", minMut = 5, pvalMethod = 'poisson')
head(onc.sig.1)


