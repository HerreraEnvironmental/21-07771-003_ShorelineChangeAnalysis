## prof 17 f20 out
## prof 17 s21 out


## Profile 17, f20 
# Duplicates, look like a tiny difference, rounding error of some kind. Question is, when did this date actually occur?
prof_17_f20_s97sp19 <- read.table("data_raw/crlc_prof_xyz_out_files_s97-sp19/prof_17_f20.out",
                          header = FALSE, col.names = c("x", "y", "z"))

prof_17_f20_sp19s22 <- read.table("data_raw/crlc_prof_xyz_out_files_sp19-s22/prof_17_f20.out",
                                   header = FALSE, col.names = c("x", "y", "z"))
all.equal(prof_17_f20_s97sp19, prof_17_f20_sp19s22)


## Profile 17, f21
prof_17_s21_s97sp19 <- read.table("data_raw/crlc_prof_xyz_out_files_s97-sp19/prof_17_s21.out",
                                  header = FALSE, col.names = c("x", "y", "z"))

prof_17_s21_sp19s22 <- read.table("data_raw/crlc_prof_xyz_out_files_sp19-s22/prof_17_s21.out",
                                  header = FALSE, col.names = c("x", "y", "z"))

all.equal(prof_17_f20_s97sp19, prof_17_f20_sp19s22)



