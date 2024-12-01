####通过桑基图描述物种分类关系及丰度
#读入数据

install.packages(c("reshape2","networkD3"))


setwd("D:\\R\\test")
genus <- read.delim('test.txt', stringsAsFactors = FALSE, check.names = FALSE)
genus
#整合分类和丰度的嵌套关系，构建桑基图 link 列表
family_genus <- genus[c('family', 'genus', 'abundance')]
names(family_genus) <- c('source', 'target', 'abundance')
order_family <- aggregate(genus$abundance, by = list(genus$order, genus$family), FUN = sum)
names(order_family) <- c('source', 'target', 'abundance')
class_order <- aggregate(genus$abundance, by = list(genus$class, genus$order), FUN = sum)
names(class_order) <- c('source', 'target', 'abundance')
phylum_class <- aggregate(genus$abundance, by = list(genus$phylum, genus$class), FUN = sum)
names(phylum_class) <- c('source', 'target', 'abundance')

link_list <- rbind(phylum_class, class_order, order_family, family_genus)

#构建 node 列表，并为 link 列表中的分类名称分配 id 指代
node_list <- reshape2::melt(genus, id = 'abundance')
node_list <- node_list[!duplicated(node_list$value),-1]
head(node_list)

link_list$IDsource <- match(link_list$source, node_list$value) - 1 
link_list$IDtarget <- match(link_list$target, node_list$value) - 1
head(link_list)

#networkD3 包的桑基图
library(networkD3)

p <- sankeyNetwork(Links = link_list, Nodes = node_list,
                   Source = 'IDsource', Target = 'IDtarget', Value = 'abundance', 
                   NodeID = 'value', NodeGroup = 'variable', 
                   fontSize = 12, sinksRight = FALSE)

p



