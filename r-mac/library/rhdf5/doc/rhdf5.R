## ----cleanup, echo=FALSE, include=FALSE------------------------------------
if(file.exists('myhdf5file.h5'))
    file.remove('myhdf5file.h5')
if(file.exists('newfile.h5'))
    file.remove('newfile.h5')
if(file.exists('newfile2.h5'))
    file.remove('newfile2.h5')
if(file.exists('newfile3.h5'))
    file.remove('newfile3.h5')

## ----installation,eval=FALSE-----------------------------------------------
#  install.packages("BiocManager")
#  BiocManager::install("rhdf5")

## ----createHDF5file--------------------------------------------------------
library(rhdf5)
h5createFile("myhdf5file.h5")

## ----create groups---------------------------------------------------------
h5createGroup("myhdf5file.h5","foo")
h5createGroup("myhdf5file.h5","baa")
h5createGroup("myhdf5file.h5","foo/foobaa")
h5ls("myhdf5file.h5")

## ----writeMatrix-----------------------------------------------------------
A = matrix(1:10,nr=5,nc=2)
h5write(A, "myhdf5file.h5","foo/A")
B = array(seq(0.1,2.0,by=0.1),dim=c(5,2,2))
attr(B, "scale") <- "liter"
h5write(B, "myhdf5file.h5","foo/B")
C = matrix(paste(LETTERS[1:10],LETTERS[11:20], collapse=""),
  nr=2,nc=5)
h5write(C, "myhdf5file.h5","foo/foobaa/C")
df = data.frame(1L:5L,seq(0,1,length.out=5),
  c("ab","cde","fghi","a","s"), stringsAsFactors=FALSE)
h5write(df, "myhdf5file.h5","df")
h5ls("myhdf5file.h5")
D = h5read("myhdf5file.h5","foo/A")
E = h5read("myhdf5file.h5","foo/B")
F = h5read("myhdf5file.h5","foo/foobaa/C")
G = h5read("myhdf5file.h5","df")

## ----accessorH5Fopen-------------------------------------------------------
h5f = H5Fopen("myhdf5file.h5")
h5f

## ----accessorDF------------------------------------------------------------
h5f$df
h5f&'df'

## ----accessorC1------------------------------------------------------------
h5f$foo$foobaa$C
h5f$"/foo/foobaa/C"

## ----accessorB1------------------------------------------------------------
h5d = h5f&"/foo/B"
h5d[]
h5d[3,,]

## ----accessorB2------------------------------------------------------------
h5d[3,,] = 1:4
H5Fflush(h5f)

## ----accessorB3,eval=FALSE-------------------------------------------------
#  h5f$foo$B = 101:120
#  h5f$"/foo/B" = 101:120

## ----accessorClose1--------------------------------------------------------
H5Dclose(h5d)
H5Fclose(h5f)

## ----accessorClose2--------------------------------------------------------
h5closeAll()

## ----writeMatrixSubsetting-------------------------------------------------
h5createDataset("myhdf5file.h5", "foo/S", c(5,8), 
                storage.mode = "integer", chunk=c(5,1), level=7)
h5write(matrix(1:5,nr=5,nc=1), file="myhdf5file.h5", 
        name="foo/S", index=list(NULL,1))
h5read("myhdf5file.h5", "foo/S")
h5write(6:10, file="myhdf5file.h5",
        name="foo/S", index=list(1,2:6))
h5read("myhdf5file.h5", "foo/S")
h5write(matrix(11:40,nr=5,nc=6), file="myhdf5file.h5", 
        name="foo/S", index=list(1:5,3:8))
h5read("myhdf5file.h5", "foo/S")
h5write(matrix(141:144,nr=2,nc=2), file="myhdf5file.h5", 
        name="foo/S", index=list(3:4,1:2))
h5read("myhdf5file.h5", "foo/S")
h5write(matrix(151:154,nr=2,nc=2), file="myhdf5file.h5", 
        name="foo/S", index=list(2:3,c(3,6)))
h5read("myhdf5file.h5", "foo/S")
h5read("myhdf5file.h5", "foo/S", index=list(2:3,2:3))
h5read("myhdf5file.h5", "foo/S", index=list(2:3,c(2,4)))
h5read("myhdf5file.h5", "foo/S", index=list(2:3,c(1,2,4,5)))

## ----writeMatrixHyperslab--------------------------------------------------
h5createDataset("myhdf5file.h5", "foo/H", c(5,8), storage.mode = "integer",
                chunk=c(5,1), level=7)
h5write(matrix(1:5,nr=5,nc=1), file="myhdf5file.h5", name="foo/H", 
        start=c(1,1))
h5read("myhdf5file.h5", "foo/H")
h5write(6:10, file="myhdf5file.h5", name="foo/H",
        start=c(1,2), count=c(1,5))
h5read("myhdf5file.h5", "foo/H")
h5write(matrix(11:40,nr=5,nc=6), file="myhdf5file.h5", name="foo/H", 
        start=c(1,3))
h5read("myhdf5file.h5", "foo/H")
h5write(matrix(141:144,nr=2,nc=2), file="myhdf5file.h5", name="foo/H", 
        start=c(3,1))
h5read("myhdf5file.h5", "foo/H")
h5write(matrix(151:154,nr=2,nc=2), file="myhdf5file.h5", name="foo/H",
        start=c(2,3), stride=c(1,3))
h5read("myhdf5file.h5", "foo/H")
h5read("myhdf5file.h5", "foo/H", 
       start=c(2,2), count=c(2,2))
h5read("myhdf5file.h5", "foo/H", 
       start=c(2,2), stride=c(1,2),count=c(2,2))
h5read("myhdf5file.h5", "foo/H", 
       start=c(2,1), stride=c(1,3),count=c(2,2), block=c(1,2))

## ----h5save----------------------------------------------------------------
A = 1:7;  B = 1:18; D = seq(0,1,by=0.1)
h5save(A, B, D, file="newfile2.h5")
h5dump("newfile2.h5")

## ----h5ls------------------------------------------------------------------
h5ls("myhdf5file.h5")
h5ls("myhdf5file.h5", all=TRUE)
h5ls("myhdf5file.h5", recursive=2)

## ----h5dump----------------------------------------------------------------
h5dump("myhdf5file.h5",load=FALSE)
D <- h5dump("myhdf5file.h5")

## ----h5dump2, eval=FALSE---------------------------------------------------
#  system("h5dump myhdf5file.h5")

## ----h5delete1-------------------------------------------------------------
h5ls("myhdf5file.h5", recursive=2)
file.size("myhdf5file.h5")

## ----h5delete2-------------------------------------------------------------
h5delete(file = "myhdf5file.h5", name = "df")
h5ls("myhdf5file.h5", recursive=2)

## ----h5delete3-------------------------------------------------------------
h5delete(file = "myhdf5file.h5", name = "foo")
h5ls("myhdf5file.h5", recursive=2)
file.size("myhdf5file.h5")

## ----bit64integer1---------------------------------------------------------
x = h5createFile("newfile3.h5")

D = array(1L:30L,dim=c(3,5,2))
d = h5createDataset(file="newfile3.h5", dataset="D64", dims=c(3,5,2),H5type="H5T_NATIVE_INT64")
h5write(D,file="newfile3.h5",name="D64")

## ----bit64integer2---------------------------------------------------------
D64a = h5read(file="newfile3.h5",name="D64",bit64conversion="int")
D64a
storage.mode(D64a)

## ----bit64integer3---------------------------------------------------------
D64b = h5read(file="newfile3.h5",name="D64",bit64conversion="double")
D64b
storage.mode(D64b)

## ----bit64integer4---------------------------------------------------------
D64c = h5read(file="newfile3.h5",name="D64",bit64conversion="bit64")
D64c
class(D64c)

## ----createfile,quiet=FALSE------------------------------------------------
library(rhdf5)
h5file = H5Fcreate("newfile.h5")
h5file

## ----create_groups, quiet=FALSE--------------------------------------------
h5group1 <- H5Gcreate(h5file, "foo")
h5group2 <- H5Gcreate(h5file, "baa")
h5group3 <- H5Gcreate(h5group1, "foobaa")
h5group3

## ----createdataspace, quiet=FALSE------------------------------------------
d = c(5,7)
h5space1 = H5Screate_simple(d,d)
h5space2 = H5Screate_simple(d,NULL)
h5space3 = H5Scopy(h5space1)
h5space4 = H5Screate("H5S_SCALAR")
h5space1
H5Sis_simple(h5space1)

## ----create_dataset--------------------------------------------------------
h5dataset1 = H5Dcreate( h5file, "dataset1", "H5T_IEEE_F32LE", h5space1 )
h5dataset2 = H5Dcreate( h5group2, "dataset2", "H5T_STD_I32LE", h5space1 )
h5dataset1

## ----writedata-------------------------------------------------------------
A = seq(0.1,3.5,length.out=5*7)
H5Dwrite(h5dataset1, A)
B = 1:35
H5Dwrite(h5dataset2, B)

## ----closefile-------------------------------------------------------------
H5Dclose(h5dataset1)
H5Dclose(h5dataset2)

H5Sclose(h5space1)
H5Sclose(h5space2)
H5Sclose(h5space3)
H5Sclose(h5space4)

H5Gclose(h5group1)
H5Gclose(h5group2)
H5Gclose(h5group3)

H5Fclose(h5file)

## ----sessioninfo-----------------------------------------------------------
sessionInfo()

## ----cleanup_after, echo=FALSE, include=FALSE------------------------------
for(file in c('myhdf5file.h5', 'newfile.h5', 'newfile2.h5', 'newfile3.h5')) {
    if(file.exists(file)) {
        file.remove( file )
    }
}

