library(doMC)
library(data.table)
registerDoMC(4)

# ---- For training set

tr <- fread("training.csv")
tr2 = subset(tr, select=-c(m2379.76:m2352.76))
setnames(tr2, gsub("\\.","_",names(tr2)))
target = subset(tr2, select=c(Ca:Sand))
tr2 <- subset(tr2, select=-c(Ca:Sand))

tr2 <- data.frame(tr2)
tr2[is.na(tr2)] <- 0
x <- foreach (i=1:length(names(tr2)), .combine=data.table) %dopar% { tr2[,i] <- paste0(names(tr2)[i], ":", tr2[,i]) }


# -- Saved as csv training_full at this point -- #

ids <- paste0(" '", tr$PIDN, "|n ")

x$result.3565 = paste0(" |m ", x$result.3565)
x$result.3580 = paste0("Depth:",as.numeric(as.factor(x$result.3580)))
x$result.1 <- NULL


createdt <- function(var) {
	ind = which(names(target)==var)
	x2 = cbind(target[,ind, with=F], ids, x)
	filename = paste0(var,"_Single_File.csv")
	write.table(x2, file=filename, row.names=F, col.names=F, sep=",")
	print ("Done ...")
	}

createdt("Ca")
createdt("P")
createdt("pH")
createdt("SOC")
createdt("Sand")


# ---- For Test Set

te <- fread("sorted_test.csv")

te2 = subset(te, select=-c(m2379.76:m2352.76))
setnames(te2, gsub("\\.","_",names(tr2)))

te2 <- data.frame(te2)
te2[is.na(te2)] <- 0

tex <- foreach (i=1:length(names(te2)), .combine=data.table) %dopar% { te2[,i] <- paste0(names(te2)[i], ":", te2[,i]) }


tids <- paste0("0 '",te$PIDN, "|n ")

tex$result.3565 = paste0(" |m ", tex$result.3565)
tex$result.3580 = paste0("Depth:",as.numeric(as.factor(tex$result.3580)))
tex$result.1 <- NULL

tex2 = cbind(tids, tex)
write.table(tex2, file="sorted_test_updated.csv", row.names=F, col.names=F, sep=",")

write.table(te$PIDN, file="test_ids.csv", row.names=F, col.names=F, sep=",")

# -------------------------------


# -------------------------------------
# Post Processing and doing predictions
# -------------------------------------

# In shell
cat Ca_Single_File.csv | sed 's/"//g' | sed 's/,/ /g' > Ca.vw

cat Sand_Single_File.csv | sed 's/"//g' | sed 's/,/ /g' > Sand.vw

cat P_Single_File.csv | sed 's/"//g' | sed 's/,/ /g' > P.vw

cat SOC_Single_File.csv | sed 's/"//g' | sed 's/,/ /g' > SOC.vw

cat pH_Single_File.csv | sed 's/"//g' | sed 's/,/ /g' > pH.vw




vw -c -k -d Ca.vw --loss_function=squared --passes=3000 -f Ca.model
echo "------------------------------------"
vw -c -k -d P.vw --loss_function=squared --passes=1000 -f P.model
echo "------------------------------------"
vw -c -k -d Sand.vw --loss_function=squared --passes=1000 -f Sand.model
echo "------------------------------------"
vw -c -k -d SOC.vw --loss_function=squared --passes=1000 -f SOC.model
echo "------------------------------------"
vw -c -k -d pH.vw --loss_function=squared --passes=1000 -f pH.model
echo "------------------------------------"

vw -c -k -t test.vw -i Ca.model -p Ca.preds
vw -c -k -t test.vw -i P.model -p P.preds
vw -c -k -t test.vw -i pH.model -p pH.preds
vw -c -k -t test.vw -i SOC.model -p SOC.preds
vw -c -k -t test.vw -i Sand.model -p Sand.preds

# ---

In R

ids = fread("test_ids.csv", header=F)
ca = fread("Ca.preds", header=F)
p = fread("P.preds", header=F)
ph = fread("pH.preds", header=F)
soc = fread("SOC.preds", header=F)
sand = fread("Sand.preds", header=F)

res = data.table(PIDN=ids$V1, Ca=ca$V1, P=p$V1, pH=ph$V1, SOC=soc$V1, Sand=sand$V1)
write.csv(res, "submit1.csv", row.names=F)




