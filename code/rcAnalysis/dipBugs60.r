# Invoca WinBugs desde R

library("arm")
library ("MCMCpack")
library (foreign)
library (car)
library (gtools)

#library (R2WinBUGS)
library (R2jags)
#library(BRugs)

rm(list = ls())
##
#workdir <- c("~/Dropbox/data/rollcall/DipMex")
workdir <- c("d:/01/Dropbox/data/rollcall/DipMex")
#workdir <- c("C:/Documents and Settings/emagarm/Mis documentos/My Dropbox/data/rollcall/DipMex")
#workdir <- c("C:/Documents and Settings/emm/Mis documentos/My Dropbox/data/rollcall/DipMex")
setwd(workdir)
##
#set.seed(1970)

load(file=paste(workdir, "votesForWeb/rc60.RData", sep="/"))
#
## ADDS VOTE ID
votdat$votid <- 1:nrow(votdat)
#
## UNTIL rc60.RData RE-READS DIPUTADO INFO (PREPARED IN EXCEL), THIS IS NOT REDUNDANT
setwd(paste(workdir, "/diputados", sep=""))
dipdat <- read.csv("dip60.csv", header=TRUE)
setwd(workdir)
#
dipdat$id <- as.character(dipdat$id)
dipdat$part <- as.character(dipdat$part)
dipdat$part[dipdat$part=="asd"] <- "psd"
##
## PARA IMPORTAR FOLIO IDENTIFICADOR DE INICIATIVAS
infopal <- c( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14,
15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46,
47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62,
63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78,
79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94,
95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107,
108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119,
120, 121, 122, 123, 124, 125, 126, 127, 128, 129, 130, 131,
132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143,
144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155,
156, 157, 158, 159, 160, 161, 162, 163, 164, 165, 166, 167,
168, 169, 170, 171, 172, 173, 174, 175, 176, 177, 178, 179,
180, 181, 182, 183, 184, 185, 186, 187, 188, 189, 190, 191,
192, 193, 194, 195, 196, 197, 198, 199, 200, 201, 202, 203,
204, 205, 206, 207, 208, 209, 210, 211, 212, 213, 214, 215,
216, 217, 218, 219, 220, 221, 222, 223, 224, 225, 226, 227,
228, 229, 230, 231, 232, 233, 234, 235, 236, 237, 238, 239,
240, 241, 242, 243, 244, 245, 246, 247, 248, 249, 250, 251,
252, 253, 254, 255, 256, 257, 258, 259, 260, 261, 262, 263,
265, 266, 267, 268, 269, 270, 271, 272, 273, 274, 275, 276,
277, 278, 279, 280, 281, 282, 283, 284, 285, 286, 287, 288,
289, 290, 291, 292, 293, 294, 295, 296, 297, 310, 311, 312,
313, 314, 315, 316, 317, 318, 319, 320, 321, 322, 323, 324,
325, 326, 327, 328, 329, 330, 331, 332, 333, 334, 335, 336,
337, 338, 339, 340, 341, 342, 343, 344, 345, 346, 347, 348,
349, 350, 351, 352, 353, 354, 355, 356, 357, 358, 359, 360,
361, 362, 363, 364, 365, 366, 367, 368, 369, 370, 371, 372,
373, 374, 375, 376, 377, 378, 379, 380, 381, 382, 383, 384,
385, 386, 387, 388, 389, 390, 391, 392, 393, 394, 395, 396,
397, 398, 399, 400, 401, 402, 403, 404, 405, 406, 407, 408,
409, 410, 411, 412, 413, 414, 415, 416, 417, 418, 419, 420,
421, 422, 423, 424, 425, 426, 427, 428, 429, 430, 431, 432,
433, 434, 435, 436, 437, 438, 439, 440, 441, 442, 443, 444,
445, 446, 447, 448, 449, 450, 451, 452, 453, 454, 455, 456,
457, 458, 459, 460, 461, 462, 463, 464, 465, 466, 467, 468,
469, 470, 471, 472, 473, 474, 475, 476, 477, 478, 479, 480,
481, 482, 483, 484, 485, 486, 487, 488, 489, 490, 491, 492,
493, 494, 495, 496, 497, 498, 499, 500, 501, 502, 503, 504,
505, 506, 507, 508, 509, 510, 511, 512, 513, 514, 515, 516,
517, 518, 519, 520, 521, 522, 523, 524, 525, 526, 527, 528,
529, 530, 531, 532, 533, 534, 535, 536, 537, 538, 539, 540,
541, 542, 543, 544, 545, 546, 547, 548, 549, 550, 551, 552,
553, 554, 555, 556, 557, 558, 559, 560, 561, 562, 563, 564,
565, 566, 567, 568, 569, 570, 571, 572, 573, 574, 575, 576,
577, 579, 580, 581, 582, 583, 584, 585, 586, 587, 588, 589,
590, 591, 592, 593, 594, 595, 596, 597, 598, 599, 600, 601,
602, 603, 604)
##
tmp <- as.numeric( sub(votdat$filename, pattern=".*sil-", replacement="") ) - infopal
sum(tmp) ## MUST BE ZERO FOR NEXT TO BE VALID
##
votdat$folio <- c( 1329, 1330, 1331, 1332, 1333, 1334, 1335, 1336,
1337, 1338, 1339, 1340, 1341, 1342, 1343, 1344, 1345, 1346,
1347, 1348, 1349, 1350, 1351, 1352, 1353, 1354, 1355, 1356,
1357, 1358, 1359, 1360, 1361, 1362, 1363, 1364, 1365, 1366,
1367, 1368, 1369, 1370, 1371, 1372, 1373, 1374, 1375, 1376,
1377, 1378, 1379, 1380, 1381, 1382, 1383, 1384, 1385, 1386,
1387, 1388, 1389, 1390, 1391, 1392, 1393, 1394, 1395, 1396,
1397, 1398, 1399, 1400, 1401, 1402, 1403, 1404, 1405, 1406,
1407, 1408, 1409, 1410, 1411, 1412, 1413, 1414, 1415, 1416,
1417, 1418, 1419, 1420, 1421, 1422, 1423, 1424, 1425, 1426,
1427, 1428, 1429, 1430, 1431, 1432, 1433, 1434, 1435, 1436,
1437, 1438, 1439, 1440, 1441, 1442, 1443, 1444, 1445, 1446,
1447, 1448, 1449, 1450, 1451, 1452, 1453, 1454, 1455, 1456,
1457, 1458, 1459, 1460, 1461, 1462, 1463, 1464, 1465, 1466,
1467, 1468, 1469, 1470, 1471, 1472, 1473, 1474, 1475, 1476,
1477, 1478, 1479, 1480, 1481, 1482, 1483, 1484, 1485, 1486,
1487, 1488, 1489, 1490, 1491, 1492, 1493, 1494, 1495, 1496,
1497, 1498, 1499, 1500, 1501, 1502, 1503, 1504, 1505, 1506,
1507, 1508, 1509, 1510, 1511, 1512, 1513, 1514, 1515, 1516,
1517, 1518, 1519, 1520, 1521, 1522, 1523, 1524, 1525, 1526,
1527, 1528, 1529, 1530, 1531, 1532, 1533, 1534, 1535, 1536,
1537, 1538, 1539, 1540, 1541, 1542, 1543, 1544, 1545, 1546,
1547, 1548, 1549, 1550, 1551, 1552, 1553, 1554, 1555, 1556,
1557, 1558, 1559, 1560, 1561, 1562, 1563, 1564, 1565, 1566,
1567, 1568, 1569, 1570, 1571, 1572, 1573, 1574, 1575, 1576,
1577, 1578, 1579, 1580, 1581, 1582, 1583, 1584, 1585, 1586,
1587, 1588, 1589, 1590, 1591, 1592, 1593, 1594, 1595, 1596,
1597, 1598, 1599, 1600, 1601, 1602, 1603, 1604, 1605, 1606,
1607, 1608, 1609, 1610, 1611, 1612, 1613, 1614, 1615, 1616,
1617, 1618, 1619, 1620, 1621, 1622, 1623, 1624, 1625, 1626,
1627, 1628, 1629, 1630, 1631, 1632, 1633, 1634, 1635, 1636,
1637, 1638, 1639, 1640, 1641, 1642, 1643, 1644, 1645, 1646,
1647, 1648, 1649, 1650, 1651, 1652, 1653, 1654, 1655, 1656,
1657, 1658, 1659, 1660, 1661, 1662, 1663, 1664, 1665, 1666,
1667, 1668, 1669, 1670, 1671, 1672, 1673, 1674, 1675, 1676,
1677, 1678, 1679, 1680, 1681, 1682, 1683, 1684, 1685, 1686,
1687, 1688, 1689, 1690, 1691, 1692, 1693, 1694, 1695, 1696,
1697, 1698, 1699, 1700, 1701, 1702, 1703, 1704, 1705, 1706,
1707, 1708, 1709, 1710, 1711, 1712, 1713, 1714, 1715, 1716,
1717, 1718, 1719, 1720, 1721, 1722, 1723, 1724, 1725, 1726,
1727, 1728, 1729, 1730, 1731, 1732, 1733, 1734, 1735, 1736,
1737, 1738, 1739, 1740, 1741, 1742, 1743, 1744, 1745, 1746,
1747, 1748, 1749, 1750, 1751, 1752, 1753, 1754, 1755, 1756,
1757, 1758, 1759, 1760, 1761, 1762, 1763, 1764, 1765, 1766,
1767, 1768, 1769, 1770, 1771, 1772, 1773, 1774, 1775, 1776,
1777, 1778, 1779, 1780, 1781, 1782, 1783, 1784, 1785, 1786,
1787, 1788, 1789, 1790, 1791, 1792, 1793, 1794, 1795, 1796,
1797, 1798, 1799, 1800, 1801, 1802, 1803, 1804, 1805, 1806,
1807, 1808, 1809, 1810, 1811, 1812, 1813, 1814, 1815, 1816,
1817, 1818, 1819, 1820, 1821, 1822, 1823, 1824, 1825, 1826,
1827, 1828, 1829, 1830, 1831, 1832, 1833, 1834, 1835, 1836,
1837, 1838, 1839, 1840, 1841, 1842, 1843, 1844, 1845, 1846,
1847, 1848, 1849, 1850, 1851, 1852, 1853, 1854, 1855, 1856,
1857, 1858, 1859, 1860, 1861, 1862, 1863, 1864, 1865, 1866,
1867, 1868, 1869, 1870, 1871, 1872, 1873, 1874, 1875, 1876,
1877, 1878, 1879, 1880, 1881, 1882, 1883, 1884, 1885, 1886,
1887, 1888, 1889, 1890, 1891, 1892, 1893, 1894, 1895, 1896,
1897, 1898, 1899, 1900, 1901, 1902, 1903, 1904, 1905, 1906,
1907, 1908, 1909, 1910, 1911, 1912, 1913, 1914, 1915, 1916,
1917, 1918)
##
votdat$VID <- c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 1194, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 1205, 0, 1208, 1209, 1210, 1211, 1212, 1213 ,
1214, 1195, 1196, 1197, 1198, 1199, 0, 0, 0, 1200, 1201, 0,
1202, 1203, 1204, 1206, 1207, 1215, 0, 0, 1226, 1237, 1248,
1259, 1269, 0, 1270, 0, 1271, 1272, 1216, 0, 0, 1217, 0, 0, 0 ,
0, 1218, 1219, 0, 1220, 1221, 1222, 1223, 1224, 1225, 1227,
1228, 1229, 1230, 1231, 0, 0, 0, 1232, 1233, 0, 0, 0, 0, 1234 ,
1235, 1236, 1238, 1239, 1240, 1241, 1242, 1243, 1244, 1245,
1246, 1247, 0, 1249, 1250, 1251, 1252, 1253, 1254, 1255, 1256 ,
1257, 0, 1258, 1260, 0, 1261, 1262, 1263, 1264, 1265, 1266,
1267, 1268, 0, 0, 1288, 1298, 1300, 1311, 1322, 1333, 1344,
1355, 1366, 0, 0, 0, 0, 0, 1289, 1290, 1291, 1292, 1293, 1294 ,
1295, 1296, 1297, 0, 0, 0, 0, 0, 0, 0, 1299, 0, 0, 0, 1301,
1302, 1303, 1304, 1305, 1306, 1307, 1308, 1309, 1310, 1312,
1313, 1314, 1315, 0, 0, 1316, 0, 1317, 1318, 1319, 1320, 1321 ,
1323, 1324, 1325, 1326, 1327, 1328, 0, 0, 1329, 1330, 1331, 0 ,
1332, 1334, 1335, 0, 0, 0, 1336, 1337, 0, 0, 1338, 1339, 1340 ,
1341, 1342, 1343, 1345, 0, 1346, 1347, 1348, 0, 1349, 1350,
1351, 1352, 1353, 1354, 0, 1356, 1357, 1358, 0, 1359, 1360,
1361, 1362, 1363, 1364, 1365, 1367, 1368, 1369, 1370, 1371,
1372, 1373, 1374, 1375, 1376, 0, 0, 1377, 1387, 1398, 1409,
1420, 1431, 0, 1432, 0, 0, 0, 1433, 0, 0, 1434, 1378, 0, 0,
1379, 1380, 1381, 1382, 0, 1383, 1384, 1385, 1386, 1388, 0,
1389, 0, 0, 1390, 1391, 1392, 1393, 1394, 1395, 1396, 1397,
1399, 1400, 1401, 1402, 1403, 0, 1404, 1405, 1406, 1407, 1408 ,
1410, 1411, 1412, 0, 0, 1413, 1414, 1415, 1416, 1417, 1418,
1419, 1421, 1422, 1423, 1424, 1425, 1426, 1427, 1428, 1429,
1430, 1273, 1274, 1275, 1276, 1277, 1278, 1279, 1280, 1281,
1282, 1283, 1284, 1285, 1286, 1287, 0, 1435, 0, 1440, 1451, 0 ,
1462, 1473, 1484, 1495, 1506, 1517, 1436, 1437, 1438, 0, 0, 0 ,
0, 0, 0, 1439, 1441, 1442, 1443, 1444, 1445, 1446, 1447, 1448 ,
1449, 1450, 1452, 1453, 1454, 1455, 1456, 1457, 1458, 1459,
1460, 1461, 1463, 1464, 0, 1465, 1466, 1467, 1468, 1469, 1470 ,
1471, 1472, 1474, 1475, 1476, 1477, 1478, 1479, 1480, 1481,
1482, 1483, 1485, 0, 0, 1486, 1487, 1488, 1489, 1490, 0, 1491 ,
1492, 1493, 1494, 1496, 1497, 1498, 1499, 1500, 1501, 1502,
1503, 1504, 1505, 1507, 1508, 1509, 1510, 0, 1511, 1512, 1513 ,
1514, 1515, 1516, 1518, 1519, 1520, 1521, 0, 0, 0, 1522, 1572 ,
1583, 1594, 1605, 1616, 1627, 1638, 1649, 1523, 0, 1534, 1544 ,
0, 1555, 1566, 1567, 1568, 1569, 1570, 1571, 1573, 1574, 1575 ,
1576, 1577, 1578, 1579, 1580, 1581, 1582, 1584, 1585, 1586,
1587, 0, 1588, 1589, 1590, 1591, 1592, 1593, 1595, 1596, 0,
1597, 1598, 1599, 0, 0, 1600, 1601, 1602, 1603, 1604, 1606,
1607, 1608, 1609, 1610, 1611, 1612, 1613, 1614, 1615, 1617,
1618, 1619, 1620, 1621, 1622, 1623, 1624, 1625, 1626, 1628,
1629, 1630, 1631, 1632, 1633, 1634, 1635, 1636, 1637, 1639,
1640, 1641, 1642, 1643, 0, 1644, 1645, 1646, 1647, 1648, 1650 ,
1651, 1652, 1653, 1654, 1655, 1656, 1657, 1658, 1659, 1524,
1525, 1526, 1527, 1528, 1529, 1530, 0, 1531, 1532, 1533, 1535 ,
1536, 1537, 1538, 1539, 1540, 0, 1541, 1542, 1543, 1545, 1546 ,
1547, 1548, 1549, 1550, 1551, 1552, 0, 0, 1553, 1554, 1556,
1557, 1558, 1559, 1560, 1561, 1562, 1563, 1564, 1565)
##
rm(infopal, tmp)

coord <- rep("", times=1000)
coord[dipdat$id=="df02p"] <- "Glez Garza"  ## prd
coord[dipdat$id=="dfrp15s"] <- "Iturbe"    ## conve
coord[dipdat$id=="dfrp18p"] <- "Lavara"    ## pvem
coord[dipdat$id=="dfrp23p"] <- "Arvizu"    ## psd
coord[dipdat$id=="nlrp05p"] <- "Cantú"     ## pt
coord[dipdat$id=="sonrp03p"] <- "Larios"   ## pan
coord[dipdat$id=="yucrp02p"] <- "Gamboa"   ## pri
coord[dipdat$id=="dfrp12p"] <- "Jiménez"   ## panal1 06-08
coord[dipdat$id=="agsrp06p"] <- "Luna"     ## panal2 08-09
##
dcoord <- rep(NA, times=1000)
dcoord[dipdat$id=="df02p"] <- 1      ## prd
dcoord[dipdat$id=="dfrp15s"] <- 1    ## conve
dcoord[dipdat$id=="dfrp18p"] <- 1    ## pvem
dcoord[dipdat$id=="dfrp23p"] <- 1    ## psd
dcoord[dipdat$id=="nlrp05p"] <- 1    ## pt
dcoord[dipdat$id=="sonrp03p"] <- 1   ## pan
dcoord[dipdat$id=="yucrp02p"] <- 1   ## pri
dcoord[dipdat$id=="dfrp12p"] <- 1    ## panal1 06-08
dcoord[dipdat$id=="agsrp06p"] <- 1   ## panal2 08-09
##
color <- rep(".", times=1000)
color[dipdat$part=="pan"] <- "darkblue"
color[dipdat$part=="pri"] <- "forestgreen"
color[dipdat$part=="prd"] <- "gold"
color[dipdat$part=="pvem"] <- "darkolivegreen2"
color[dipdat$part=="conve"] <- "orange"
color[dipdat$part=="panal"] <- "cyan"
color[dipdat$part=="pt"] <- "red"
color[dipdat$part=="psd"] <- "darkorchid"
##
part.list <- c("PAN", "PRI", "PRD", "PT", "PVEM",
                "Conv.", "PANAL", "PSD")
color.list <- c("darkblue", "forestgreen", "gold",
                "red", "darkolivegreen2", "orange",
                "cyan", "darkorchid")
##
dipdat$color <- as.character(color)
dipdat$dcoord <- dcoord
dipdat$coord <- coord
rm(color, dcoord, coord)
##
## RECODE RCs TO -1=nay 0=abstain/absent 1=aye
rc <- apply(rc, 2, recode, recodes="2=-1; 3=0; 4=0; 5=0")
##

## VOTE AGGREGATES FUNCTION
count.votes <- function (X)
{
   ayes <- length(X[is.na(X)==FALSE & X==1])
   nays <- length(X[is.na(X)==FALSE & X==-1])
   valid <- ayes+nays
   abs <- length(X[is.na(X)==FALSE & X==0])
   tot <- valid+abs
   categ <- c (ayes, nays, valid, abs, tot)
   return (categ)
}
##
names(votdat)[1:5] <- c("ayes","nays","valid","abs","tot") ## CHANGE VOTE STATS
I <- dim(rc)[1]; J <- dim(rc)[2]
for (i in 1:I){
    votdat$ayes[i] <- count.votes(rc[i,])[1]
    votdat$nays[i] <- count.votes(rc[i,])[2]
    votdat$valid[i] <- count.votes(rc[i,])[3]
    votdat$abs[i] <- count.votes(rc[i,])[4]
    votdat$tot[i] <- count.votes(rc[i,])[5]
              }
##

##
#for(i in 1:I){
#    print(colnames(rc)[i])
#    print(table(rc[,i]))
#    }

## PROPIETARIO/SUPLENTE/NEITHER WAS MEMBER
votdat$date <- as.Date(paste(votdat$yr[1:I],votdat$mo[1:I],votdat$dy[1:I],sep="-"))
## IF NEED TO SEE DATES; USE FOLLOWING COMMAND
#votdat$date <- format(votdat$date, format="%d %b %y")
#
#### DIPUTADOS 60 FILE HAS INCOMPLETE DATES AND SOME IN WRONG COLUMN... CLEAN IF THIS IS TO BE USED
##dipdat$in1 <- rep(NA,1000); dipdat$out1 <- rep(NA,1000); dipdat$in2  <- rep(NA,1000); dipdat$out2 <- rep(NA,1000)
##for (j in 1:1000){
##    dipdat$in1[j]  <- ifelse( dipdat$yrin1[j]=="." ,  NA, as.Date(paste(dipdat$yrin1[j],dipdat$moin1[j],dipdat$dyin1[j],sep="-")) )
##    dipdat$out1[j] <- ifelse( dipdat$yrout1[j]==".",  NA, as.Date(paste(dipdat$yrout1[j],dipdat$moout1[j],dipdat$dyout1[j],sep="-")) )
##    dipdat$in2[j]  <- ifelse( dipdat$yrin2[j]=="." ,  NA, as.Date(paste(dipdat$yrin2[j],dipdat$moin2[j],dipdat$dyin2[j],sep="-")) )
##    dipdat$out2[j] <- ifelse( dipdat$yrout2[j]==".",  NA, as.Date(paste(dipdat$yrout2[j],dipdat$moout2[j],dipdat$dyout2[j],sep="-")) )
##    }
#### IF NEED TO SEE DATES; USE FOLLOWING COMMAND
###dipdat$in1  <- format(dipdat$in1 , format="%d %b %y")
###dipdat$out1 <- format(dipdat$out1, format="%d %b %y")
###dipdat$in2  <- format(dipdat$in2 , format="%d %b %y")
###dipdat$out2 <- format(dipdat$out2, format="%d %b %y")
###
##dmember <- rc
##dmember[,] <- NA
###
##dmember[,is.na(dipdat$in1)==TRUE] <- 0
##for (i in 1:I){
##    dmember[i,(votdat$date[i]< dipdat$in1  & is.na(dipdat$in1)==FALSE)] <- 0;
##    dmember[i,(dipdat$in1<=votdat$date[i]  & is.na(dipdat$in1)==FALSE)] <- 1;
##    dmember[i,(dipdat$out1<=votdat$date[i] & is.na(dipdat$out1)==FALSE)] <- 0;
##    dmember[i,(dipdat$in2<=votdat$date[i]  & is.na(dipdat$in2)==FALSE)] <- 1;
##    dmember[i,(dipdat$out2<=votdat$date[i] & is.na(dipdat$out2)==FALSE)] <- 0;
##    }
##rm(i,j)

## DEPUTY'S ABSTENTION RATE OVERALL (TO DROP THOSE NOT VOTING ENOUGH OVERALL)
noVoteRate <- as.numeric(rc[1,])
for (j in 1:J){
    noVoteRate[j] <- count.votes(rc[,j])[4] / count.votes(rc[,j])[5]
#    noVoteRate[j] <- length(rc[rc[,j]==0,d])/dim(rc)[1]
    }
dipdat$noVoteRate <- noVoteRate
rm(noVoteRate)
#
#### DEPUTY'S ABSTENTION RATE WHEN CHAMBER MEMBER (FOR DESCRIPTIVE STATS)
##noVoteRateMem <- as.numeric(rc[1,])
##tmp <- rc*dmember
#### DMEMBER STILL HAS PROBLEMS, IN/OUT DATES DO NOT MATCH VOTES EXACTLY
##j <- 75; cbind(dmember[,j], rc[,j]) ## AS THIS SHOWS FOR SOME js
##for (j in 1:J){
##    dmember[abs(rc[,j])==1,j] <- 1  ## IMPERFECT BUT PRACTICAL SOLUTION: IMPLY dmember IS ONE if rc IS -1 or 1
##    }
##ttmp <- as.numeric(rc[1,])
##tmp <- rc*dmember
##for (j in 1:J){
##    ttmp[j] <- sum(dmember[,j])
##    noVoteRateMem[j] <- 1-count.votes(tmp[,j])[3] / sum(dmember[,j])
##    }
##dipdat$noVoteRateMem <- noVoteRateMem
##rm(j, tmp, ttmp, noVoteRateMem)
##
## DROP DIPUTADOS WHO NEVER PLEDGED
dropUnpledged <- rep(0, times=1000)
for (j in 1:J){
    dropUnpledged[j] <- ifelse(count.votes(rc[,j])[3]==0, -1, 0)
                 }
length(dropUnpledged[dropUnpledged<0])  ## HOW MANY NEVER PLEDGED (DESCRIPTIVES)
dropUnpledged <- dropUnpledged * (1:1000)
dropUnpledged <- dropUnpledged[dropUnpledged<0] ## NEGATIVE INDEX FOR THOSE WHO NEVER PLEDGED
rc <- rc[,dropUnpledged]; dipdat <- dipdat[dropUnpledged,]; ##dmember <- dmember[,dropUnpledged]
tmp <- rc; tmp <- t(tmp); table(tmp) ## CHECA QUE TODO SEA -1 0 1
J <- dim(rc)[2]
##
## DROP DIPUTADOS WHO VOTED IN LESS THAN 10% OF ALL VOTES
D <- dim(dipdat)[1]
dropAbstainers <- rep(0, times=D)
for (d in 1:D){
    dropAbstainers[d] <- ifelse(dipdat$noVoteRate[d]>.9, -1, 0)
                 }
##
length(dropAbstainers[dropAbstainers<0])  ## HOW MANY HAD TOO FEW VOTES (DESCRIPTIVES)
dipdat$id[dropAbstainers<0]               ## WHO THEY ARE (MOSTLY SUPLENTES)
##
dropAbstainers <- dropAbstainers * (1:D)
dropAbstainers <- dropAbstainers[dropAbstainers<0] ## NEGATIVE INDEX FOR THOSE WHO NEVER VOTED
rc <- rc[,dropAbstainers]; dipdat <- dipdat[dropAbstainers,]
rm(d, D, dropAbstainers, dropUnpledged)
##
## DROP UNCONTESTED VOTES
I <- dim(rc)[1]
dropUncontested <- ifelse(votdat$ayes==0 | votdat$nays==0, -1, 0)
##
length(dropUncontested[dropUncontested<0])  ## HOW MANY UNCONTESTED VOTES (DESCRIPTIVES)
##
dropUncontested <- dropUncontested * (1:I)
dropUncontested <- dropUncontested[dropUncontested<0] ## NEGATIVE INDEX
rc <- rc[dropUncontested,]
votdat <- votdat[dropUncontested,]
dgaceta <- dgaceta[dropUncontested]
rm(j, dropUncontested, tmp)
##
## DROP UNIFORMATIVE VOTES WITH MINORITY < 2.5%
I <- dim(rc)[1]
dropUninformative <- ifelse( votdat$ayes/votdat$valid<.025 | votdat$nays/votdat$valid<.025, -1, 0)
##
length(dropUninformative[dropUninformative<0])  ## HOW MANY UNINFORMATIVE VOTES (DESCRIPTIVES)
##
dropUninformative <- dropUninformative * (1:I)
dropUninformative <- dropUninformative[dropUninformative<0] ## NEGATIVE INDEX
rc <- rc[dropUninformative,]
votdat <- votdat[dropUninformative,]
dgaceta <- dgaceta[dropUninformative]
rm(dropUninformative)

#### WORK WITH SAMPLE OF VOTES IF SO WISHED
#rnd <- runif(I)
#rc <- rc[rnd<.2,]
#votdat <- votdat[rnd<.2,]
##rm(rnd)
###
#### WORK WITH SAMPLE OF DIPUTADOS IF SO WISHED
#rnd <- runif(J)
#rc <- rc[,rnd<.25]
#dipdat <- dipdat[rnd<.25,]
##rm(rnd)
###
## DROP VOTES NOT IN GACETA IF SO WISHED
##rc <- rc[dgaceta==1,]
##votdat <- votdat[dgaceta==1,]
###
J <- ncol(rc); I <- nrow(rc)

## ## ONE-DIM ARRANGEMENT
## ## AGREEMENT MATRIX --- LA GUARDE PORQUE ESTO TARDA AÑOS
## load("agreeMatrix.Rdata")
## #agreeMatrix <- matrix(NA, ncol=J, nrow=J); tmp <- rep(NA, times=I)
## #datt <- t(dat)
## #for (j in 1:J){
## #    agreeMatrix[j,j] <- 1  ## DIAGONAL
## #              }
## #for (j1 in 2:J){
## #    for (j2 in (j1-1):1){
## #        for (i in 1:I){
## #            tmp[i] <- ifelse(datt[i,j1]==datt[i,j2], 1, 0)
## #                      }
## #        agreeMatrix[j2,j1] <- sum(tmp)/I; agreeMatrix[j1,j2] <- agreeMatrix[j2,j1]
## #        print( paste("j1 =",j1,"; j2 =",j2) )
## #                        }
## #               }
## #rm(datt)
## #save(agreeMatrix, file="agreeMatrix.Rdata")
## ## SQUARED DISTANCES
## sd <- (1-agreeMatrix)^2
## ## DOUBLE-CENTRED MATRIX
## pmean <- rep(NA, times=J); mmat <- mean(sd); dc <- sd
## for (j in 1:J){
##     pmean[j] <- mean(sd[j,])
##               }
## for (r in 1:J){
##     for (c in 1:J){
##         dc[r,c] <- (sd[r,c] - pmean[r] - pmean[c] + mmat)/-2
##                   }
##               }
## ## SIMPLE ONE-DIM IDEAL POINTS
## tmp <- sqrt(dc[1,1])
## ip  <- c(tmp, dc[2:J,1]/tmp)
## summary(ip)
## ##
## ## EXTREMA DERECHA
## thr <- .14
## data.frame(ip=ip[c(1:J)[ip>thr]], id=dipdat$id[c(1:J)[ip>thr]], nom=dipdat$nom[c(1:J)[ip>thr]], part=dipdat$part[c(1:J)[ip>thr]], noVote=dipdat$noVoteRate[c(1:J)[ip>thr]])
## ##EXTREMA IZQUIERDA
## thr <- -.185
## data.frame(ip=ip[c(1:J)[ip<thr]], id=dipdat$id[c(1:J)[ip<thr]], nom=dipdat$nom[c(1:J)[ip<thr]], part=dipdat$part[c(1:J)[ip<thr]], noVote=dipdat$noVoteRate[c(1:J)[ip< thr]])
## ##
## tmp <- dat[,dimnames(dat)[[2]]=="dpanid" | dimnames(dat)[[2]]=="dpriid" | dimnames(dat)[[2]]=="dprdid"]
## tmp[,4] <- 1 - tmp[,1] - tmp[,2] - tmp[,3]
## dimnames(tmp)[[2]][4] <- "dothid"
## tmp[,2] <- tmp[,2]*2; tmp[,3] <- tmp[,3]*3; tmp[,4] <- tmp[,4]*4
## part <- as.vector(apply(tmp, 1, sum))
## #
## rnd <- (runif(dim(dat)[1])-.5)/10 ## random jitter
## plot(c(-.5,.3), c(1,4), type="n")
## for (j in 1:J){
##     points(ip[j], part[j]+rnd[j], pch=20, cex=.5, col=envud$color[j])
##     }
## #
## ##################################################
## ### FACTOR ANALYSIS TO ESTIMATE DIMENSIONALITY ###
## ##################################################
## #
## ### ALLOWS TO DROP CASES FROM ANALYSIS
## #year1 <- ifelse( (cuad==1 | cuad==2 | cuad==3), 1, 0 ) ## FALTA QUITAR A UN DIPUTADO (M?S?) QUE NO ENTR? HASTA DESPU?S
## #drop <- ifelse(year1==0,1,0)
## #tmp<-RCs[drop==0,]
## #RCs<-tmp
## #sem<-sem[drop==0]; cuad<-cuad[drop==0]; trim<-trim[drop==0]
## #
## #votes <- rc
## #votes[votes==-1] <- 0  # los -1s se vuelven 0s # DEJA ABSTENCION == NAY
## #votes <- t(votes)
## #votes <- votes[,1:50] ## subset to test
## #
## cor(dat)
## factanal(dat, factors=4) # varimax is the default
## #
## factanal(dat, factors=3, rotation="promax")
## #
## # A little demonstration, v2 is just v1 with noise,
## # and same for v4 vs. v3 and v6 vs. v5
## # Last four cases are there to add noise
## # and introduce a positive manifold (g factor)
## v1 <- c(1,1,1,1,1,1,1,1,1,1,3,3,3,3,3,4,5,6)
## v2 <- c(1,2,1,1,1,1,2,1,2,1,3,4,3,3,3,4,6,5)
## v3 <- c(3,3,3,3,3,1,1,1,1,1,1,1,1,1,1,5,4,6)
## v4 <- c(3,3,4,3,3,1,1,2,1,1,1,1,2,1,1,5,6,4)
## v5 <- c(1,1,1,1,1,3,3,3,3,3,1,1,1,1,1,6,4,5)
## v6 <- c(1,1,1,2,1,3,3,3,4,3,1,1,1,2,1,6,5,4)
## m1 <- cbind(v1,v2,v3,v4,v5,v6)
## cor(m1)
## factanal(m1, factors=3) # varimax is the default
## factanal(m1, factors=3, rotation="promax")
## # The following shows the g factor as PC1
## prcomp(m1)
## #
## ## formula interface
## factanal(~v1+v2+v3+v4+v5+v6, factors = 3,
##         scores = "Bartlett")$scores
## #
## ## a realistic example from Barthlomew (1987, pp. 61-65)
## example(ability.cov)

##########################################################################
###   Static 2Dimensions four deputy anchors, irt paremeterization     ###
##########################################################################

## NEEDED TO RUN IN JAGS INSTEAD OF BUGS (ABSTENTIONS AND ABSENCES AS MISSING)
rc <- apply(rc, 2, recode, recodes="-1=0; 0=NA")

## SORT BY PARTY
tmp <- 1:dim(rc)[1]
tmp <- ifelse ( dipdat$part=="pan", tmp,
        ifelse ( dipdat$part=="pri", tmp+1000,
         ifelse ( dipdat$part=="prd", tmp+2000,
          ifelse ( dipdat$part=="pt", tmp+3000,
           ifelse ( dipdat$part=="pvem", tmp+4000,
            ifelse ( dipdat$part=="conve", tmp+5000,
             ifelse ( dipdat$part=="panal", tmp+6000,
              ifelse ( dipdat$part=="psd", tmp+7000, tmp+8000 ))))))))
## rcold <- rc; dipold <- dipdat
rc <- rc[,order(tmp)]; dipdat <- dipdat[order(tmp),]
## PARTY INDICES (FOR ANCHORS)
PAN <- length(tmp[tmp<1001]); PRI <- PAN+length(tmp[tmp>1000&tmp<2001]);
PRD <- PRI+length(tmp[tmp>2000&tmp<3001]); PT <- PRD+length(tmp[tmp>3000&tmp<4001])
PVEM <- PT+length(tmp[tmp>4000&tmp<5001]); CONVE <- PVEM+length(tmp[tmp>5000&tmp<6001])
PANAL <- CONVE+length(tmp[tmp>6000&tmp<7001]); PSD <- PANAL+length(tmp[tmp>7000&tmp<8001])
##
PAN; PRI; PRD; PVEM; CONVE

## JAGS version
cat("
model {
  for (j in 1:J){                                             ## loop over diputados
    for (i in 1:I){                                           ## loop over items
      v[j,i] ~ dbern(p[j,i]);                                 ## voting rule
      probit(p[j,i]) <- mu[j,i];                              ## sets 0<p<1
      mu[j,i] <- beta[i]*x[j] - alpha[i] + delta[i]*y[j]      ## utility differential
                  }
                }
## ESTO LO PUEDO SACAR POST ESTIMACION
##  for (i in 1:I){
##  a[i] <- beta[i] / delta[i]  ## pendiente de cutline
##  b[i] <- alpha[i] / delta[i] ## constante de cutline
##  }
  ## priors ################
for (j in 1:PAN){
    x[j] ~  dnorm(1, 4)   # PAN
    y[j] ~  dnorm(-1, 4)
    }
for (j in (PAN+1):PRI){
    x[j] ~  dnorm(0, 4)    # PRI
    y[j] ~  dnorm(.5, 4)
    }
for (j in (PRI+1):PRD){
    x[j] ~  dnorm(-1, 4)    # PRD SEMI-INFORMATIVE
    y[j] ~  dnorm(0, .1)
    }
for (j in (PRD+1):PVEM){
    x[j] ~  dnorm(0, .1)    # REST UNINFORMATIVE
    y[j] ~  dnorm(0, .1)
    }
for (j in (PVEM+1):CONVE){
    x[j] ~  dnorm(-1, 4)    # CONVE
    y[j] ~  dnorm(1, 4)
    }
for (j in (CONVE+1):J){
    x[j] ~  dnorm(0, .1)    # REST UNINFORMATIVE
    y[j] ~  dnorm(0, .1)
    }
    for(i in 1:I){
        alpha[i] ~ dnorm( 0, 1)
        beta[i]  ~ dnorm( 0, 1)
        delta[i] ~ dnorm( 0, 1)
                 }
}
", file="model2Dj.irt.txt")

## ## BUGS VERSION
## cat("
## model {
##   for (j in 1:J){                ## loop over diputados
##     for (i in 1:I){              ## loop over items
##       #v.hat[j,i] ~ dbern(p[j,i]);                                  ## voting rule
##       #p[j,i] <- phi(v.star[j,i]);                                  ## sets 0<p<1
##       v.star[j,i] ~ dnorm(mu[j,i],1)I(lo.v[j,i],hi.v[j,i]);   ## truncated normal sampling
##       mu[j,i] <- beta[i]*x[j] - alpha[i] + delta[i]*y[j] ## utility differential
##                   }
##                 }
## ## ESTO LO PUEDO SACAR POST ESTIMACION
## ##  for (i in 1:I){
## ##  a[i] <- beta[i] / delta[i]  ## pendiente de cutline
## ##  b[i] <- alpha[i] / delta[i] ## constante de cutline
## ##  }
##   ## priors ################
## for (j in 1:PAN){
##     x[j] ~  dnorm(1, 4)   # PAN
##     y[j] ~  dnorm(-1, 4)
##     }
## for (j in (PAN+1):PRI){
##     x[j] ~  dnorm(0, 4)    # PRI
##     y[j] ~  dnorm(.5, 4)
##     }
## for (j in (PRI+1):PRD){
##     x[j] ~  dnorm(-1, 4)    # PRD SEMI-INFORMATIVE
##     y[j] ~  dnorm(0, .1)
##     }
## for (j in (PRD+1):PVEM){
##     x[j] ~  dnorm(0, .1)    # REST UNINFORMATIVE
##     y[j] ~  dnorm(0, .1)
##     }
## for (j in (PVEM+1):CONVE){
##     x[j] ~  dnorm(-1, 4)    # CONVE
##     y[j] ~  dnorm(1, 4)
##     }
## for (j in (CONVE+1):J){
##     x[j] ~  dnorm(0, .1)    # REST UNINFORMATIVE
##     y[j] ~  dnorm(0, .1)
##     }
##     for(i in 1:I){
##         alpha[i] ~ dnorm( 0, 1)
##         beta[i]  ~ dnorm( 0, 1)
##         delta[i] ~ dnorm( 0, 1)
##                  }
## }
## ", file="model2Dj.irt.txt")

################### ESTO EVITA TENER QUE SALVAR COMO scjn.bug ####################################
###
#####################################################################
##### static model in Two Dimensions WITH CUTLINE ESTIMATES
#####################################################################
##cat("
##model {
##  for (j in 1:J){                ## loop over diputados
##    for (i in 1:I){              ## loop over items
##      #y.hat[j,i] ~ dbern(p[j,i]);                                  ## voting rule
##      #p[j,i] <- phi(v.star[j,i]);                                  ## sets 0<p<1
##      v.star[j,i] ~ dnorm(mu[j,i],1)I(lo.v[j,i],hi.v[j,i]);   ## truncated normal sampling
##      mu[j,i] <- delta[i]*a[i]*x[j] + delta[i]*b[i] - delta[i]*y[j] ## utility differential
##                  }
##                }
##  for (i in 1:I){
##  a[i] <- sin(angle[i]) / sqrt(1-sin(angle[i])*sin(angle[i]))
##  }
##  ## priors ################
##for (j in 1:PAN){
##    x[j] ~  dnorm(1, 4)   # PAN
##    y[j] ~  dnorm(1, 4)
##    }
##for (j in (PAN+1):PRI){
##    x[j] ~  dnorm(0, 4)    # PRI
##    y[j] ~  dnorm(-1, 4)
##    }
##for (j in (PRI+1):PRD){
##    x[j] ~  dnorm(-1, 4)    # PRD
##    y[j] ~  dnorm(.5, 4)
##    }
##for (j in (PRD+1):PT){
##    x[j] ~  dnorm(-1, 4)    # PT
##    y[j] ~  dnorm(-1, 4)
##    }
##for (j in (PT+1):J){
##    x[j] ~  dnorm(0, .1)
##    y[j] ~  dnorm(0, .1)
##    }
##    for(i in 1:I){
##        delta[i] ~ dnorm( 0, 0.1)
##        angle[i] ~ dunif(-1.57,1.57) # (-pi/2,pi/2)
##        b[i] ~ dnorm( 0, .1)
##                 }
##}
##", file="modelSta2Dj.txt")
###
#####################################################################
##### static model in Two Dimensions -- IRT PARAMETERIZATION
#####################################################################
##cat("
##model {
##  for (j in 1:J){                ## loop over diputados
##    for (i in 1:I){              ## loop over items
##      #v.hat[j,i] ~ dbern(p[j,i]);                                  ## voting rule
##      #p[j,i] <- phi(v.star[j,i]);                                  ## sets 0<p<1
##      v.star[j,i] ~ dnorm(mu[j,i],1)I(lo.v[j,i],hi.v[j,i]);   ## truncated normal sampling
##      mu[j,i] <- beta[i]*x[j] - alpha[i] + delta[i]*y[j] ## utility differential
##                  }
##                }
#### ESTO LO PUEDO SACAR POST ESTIMACION
####  for (i in 1:I){
####  a[i] <- beta[i] / delta[i]  ## pendiente de cutline
####  b[i] <- alpha[i] / delta[i] ## constante de cutline
####  }
##  ## priors ################
##for (j in 1:PAN){
##    x[j] ~  dnorm(1, 4)   # PAN
##    y[j] ~  dnorm(1, 4)
##    }
##for (j in (PAN+1):PRI){
##    x[j] ~  dnorm(0, 4)    # PRI
##    y[j] ~  dnorm(-1, 4)
##    }
##for (j in (PRI+1):PRD){
##    x[j] ~  dnorm(-1, 4)    # PRD
##    y[j] ~  dnorm(.5, 4)
##    }
##for (j in (PRD+1):PT){
##    x[j] ~  dnorm(-1, 4)    # PT
##    y[j] ~  dnorm(-1, 4)
##    }
##for (j in (PT+1):J){
##    x[j] ~  dnorm(0, .1)
##    y[j] ~  dnorm(0, .1)
##    }
##    for(i in 1:I){
##        alpha[i] ~ dnorm( 0, 1)
##        beta[i]  ~ dnorm( 0, 1)
##        delta[i] ~ dnorm( 0, 1)
##                 }
##}
##", file="modelSta2Dj.irt.txt")
###################################################
##### static model in 1 dimension, extremist anchor
###################################################
##cat("
##model {
##  for (j in 1:J){                ## loop over councilors
##    for (i in 1:I){              ## loop over items
##     #v.hat[j,i] ~ dbern(p[j,i])                                   ## voting rule
##     #p[j,i] <- phi(y.star[j,i])                                   ## sets 0<p<1
##     v.star[j,i] ~ dnorm(mu[j,i],1)I(lo.v[j,i],hi.v[j,i])   ## truncated normal sampling
##     mu[j,i] <- delta[i]*x[j] - n[i]                              ## utility differential
##     }
##  }
###  for (i in 1:I){
###     m[i] <- n[i] / delta[i]                                      ## midpoint
###  }
##  ## priors
##for (j in 1:(DER-1)){
##    x[j] ~  dnorm(0, .1)
##                    }
##    x[DER] ~  dnorm(-10, 4)    # Mr. RIGHT ags01p PAN
###    x[DER] <- -1
##for (j in (DER+1):(IZQ-1)){
##    x[j] ~  dnorm(0, .1)
##                          }
##    x[IZQ] ~  dnorm(10, 4)   # Mr. LEFT Noroña PT
###    x[IZQ] <- 1
##for (j in (IZQ+1):J){
##    x[j] ~  dnorm(0, .1)
##                    }
##for (i in 1:I){
##    delta[i] ~ dnorm(0, 0.25)
##    n[i] ~ dnorm( 0, 0.25)
##              }
##}
##", file="modelSta1Dj.txt")
####
######################################################################
###### static model for 66 members in Two Dimensions, four ITEM anchors -- IRT PARAMETERIZATION
######################################################################
##cat("
##model {
##  for (j in 1:J){                ## loop over diputados
##    for (i in 1:I){              ## loop over items
##      #v.hat[j,i] ~ dbern(p[j,i]);                                  ## voting rule
##      #p[j,i] <- phi(v.star[j,i]);                                  ## sets 0<p<1
##      v.star[j,i] ~ dnorm(mu[j,i],1)T(lo.v[j,i],hi.v[j,i]);   ## truncated normal sampling
##      mu[j,i] <- beta[i]*x[j] - alpha[i] + delta[i]*y[j] ## utility differential
##                  }
##                }
#### ESTO LO PUEDO SACAR POST ESTIMACION
####  for (i in 1:I){
####  a[i] <- beta[i] / delta[i]  ## pendiente de cutline
####  b[i] <- alpha[i] / delta[i] ## constante de cutline
####  }
##  ## priors ################
##for (j in 1:J){
##    x[j] ~  dnorm(0, 1)
##    y[j] ~  dnorm(0, 1)
##    }
##for(i in 1:(west-1)){
##    alpha[i] ~ dnorm( 0, 1)
##    beta[i]  ~ dnorm( 0, 1)
##    delta[i] ~ dnorm( 0, 1)
##    }
##alpha[west] ~ dnorm( 0, 1)
##beta[west]  ~ dnorm(-4, 20)
##delta[west] ~ dnorm(-4, 20)
##for(i in (west+1):(east-1)){
##    alpha[i] ~ dnorm( 0, 1)
##    beta[i]  ~ dnorm( 0, 1)
##    delta[i] ~ dnorm( 0, 1)
##    }
##alpha[east] ~ dnorm( 0, 1)
##beta[east]  ~ dnorm(-4, 20)
##delta[east] ~ dnorm( 4, 20)
##for(i in (east+1):(north-1)){
##    alpha[i] ~ dnorm( 0, 1)
##    beta[i]  ~ dnorm( 0, 1)
##    delta[i] ~ dnorm( 0, 1)
##    }
##alpha[north] ~ dnorm( 0, 1)
##beta[north]  ~ dnorm( 4, 20)
##delta[north] ~ dnorm( 4, 20)
##for(i in (north+1):I){
##    alpha[i] ~ dnorm( 0, 1)
##    beta[i]  ~ dnorm( 0, 1)
##    delta[i] ~ dnorm( 0, 1)
##    }
##}
##", file="modelSta2Di.irt.txt")
####
####################################################
###### static model in 1 dimension, item anchors
####################################################
###cat("
###model {
###  for (j in 1:J){                ## loop over councilors
###    for (i in 1:I){              ## loop over items
###     #y.hat[j,i] ~ dbern(p[j,i])                                   ## voting rule
###     #p[j,i] <- phi(y.star[j,i])                                   ## sets 0<p<1
###     y.star[j,i] ~ dnorm(mu[j,i],1)I(lower.y[j,i],upper.y[j,i])   ## truncated normal sampling
###     mu[j,i] <- delta[i]*x[j] - n[i]                              ## utility differential
###     }
###  }
###  for (i in 1:I){
###     m[i] <- n[i] / delta[i]                                      ## midpoint
###  }
###  ## priors
###     for (j in 1:J){
###         x[j] ~ dnorm(0, .1)
###                   }
###    for(i in 1:31){
###        delta[i] ~ dnorm( 0, 0.25)
###                  }
###    delta[32] ~ dnorm( 4, 4)      ## folio 390, right=nay
###    for(i in 33:227){
###        delta[i] ~ dnorm( 0, 0.25)
###                   }
###    delta[228] ~ dnorm(-4, 4)      ## folio 1045, right=aye
###    for(i in 229:I){
###        delta[i] ~ dnorm( 0, 0.25)
###                  }
###    for(i in 1:I){
###        n[i] ~ dnorm( 0, 0.25)
###                 }
###}
###", file="modelSta1Dj.txt")
### #
###### dynamic model for 66 members in Two Dimensions WITH CUTLINE ESTIMATES
###cat("
###model {
###  for (j in 1:J){                ## loop over diputados
###    for (i in 1:I){              ## loop over items
###      #y.hat[j,i] ~ dbern(p[j,i]);                                  ## voting rule
###      #p[j,i] <- phi(v.star[j,i]);                                  ## sets 0<p<1
###      v.star[j,i] ~ dnorm(mu[j,i],1)I(lo.v[j,i],hi.v[j,i]);   ## truncated normal sampling, cf. Jackman
###      mu[j,i] <- delta[i]*a[i]*(xOne[j]*d1[i] + xTwo[j]*d2[i] + xThree[j]*d3[i] + xFour[j]*d4[i] + xFive[j]*d5[i] + xSix[j]*d6[i] + xSeven[j]*d7[i] + xEight[j]*d8[i])
###                + delta[i]*b[i] - delta[i]*(yOne[j]*d1[i] + yTwo[j]*d2[i] + yThree[j]*d3[i] + yFour[j]*d4[i] + yFive[j]*d5[i] + ySix[j]*d6[i] + ySeven[j]*d7[i] + yEight[j]*d8[i])  ## utility differential
###                  }
###      xOne[j] ~   dnorm (xZero[j],15);  ## en 2do intento slack era 20, 3ro 10
###      xTwo[j] ~   dnorm (xOne[j],15);
###      xThree[j] ~ dnorm (xTwo[j],15);
###      xFour[j] ~  dnorm (xThree[j],15);
###      xFive[j] ~  dnorm (xFour[j],15);
###      xSix[j] ~   dnorm (xFive[j],15);
###      xSeven[j] ~ dnorm (xSix[j],15);
###      xEight[j] ~ dnorm (xSeven[j],15);
###      yOne[j] ~   dnorm (yZero[j],15);
###      yTwo[j] ~   dnorm (yOne[j],15);
###      yThree[j] ~ dnorm (yTwo[j],15);
###      yFour[j] ~  dnorm (yThree[j],15);
###      yFive[j] ~  dnorm (yFour[j],15);
###      ySix[j] ~   dnorm (yFive[j],15);
###      ySeven[j] ~ dnorm (ySix[j],15);
###      yEight[j] ~ dnorm (ySeven[j],15);
###                }
###  for (i in 1:I){
###  a[i] <- sin(angle[i]) / sqrt(1-sin(angle[i])*sin(angle[i]))
###  }
###  ################
###  ## priors
###  ################
###for (j in 1:(N-1)){
###    xZero[j] ~  dnorm(0, 1)
###    yZero[j] ~  dnorm(0, 1)
###    }
###    xZero[N] ~  dnorm(0, 4)    # Mrs. NORTH Piña Olmedo Laura (PRD)
###    yZero[N] ~  dnorm(2, 4)
####    xZero[N] <- 0
####    yZero[N] <- 2
###for (j in (N+1):(W-1)){
###    xZero[j] ~  dnorm(0, 1)
###    yZero[j] ~  dnorm(0, 1)
###    }
###    xZero[W] ~  dnorm(-2, 4)   # Mr. WEST Méndez Rangel Avelino (PRD)
###    yZero[W] ~  dnorm(0, 4)
####    xZero[W] <- -2
####    yZero[W] <- 0
###for (j in (W+1):(E-1)){
###    xZero[j] ~  dnorm(0, 1)
###    yZero[j] ~  dnorm(0, 1)
###    }
###    xZero[E] ~  dnorm(2, 4)    # Mrs. EAST Paula Adriana Soto Maldonado (PAN)
###    yZero[E] ~  dnorm(0, 4)
####    xZero[E] <- 2
####    yZero[E] <- 0
###for (j in (E+1):(S-1)){
###    xZero[j] ~  dnorm(0, 1)
###    yZero[j] ~  dnorm(0, 1)
###    }
###    xZero[S] ~  dnorm(0, 4)    # Mr. SOUTH Tenorio Antiga Xiuh (PANAL)
###    yZero[S] ~  dnorm(-2, 4)
####    xZero[S] <- 0
####    yZero[S] <- -2
###for (j in (S+1):J){
###    xZero[j] ~  dnorm(0, 1)
###    yZero[j] ~  dnorm(0, 1)
###    }
###    for(i in 1:I){
###        delta[i] ~ dnorm( 0, 0.01)
###        angle[i] ~ dunif(-1.57,1.57) # (-pi/2,pi/2)
###        b[i] ~ dnorm( 0, .01)
###                 }
###}
###", file="model66Dyn2Dj.txt")
####
####
######################################################################
###### static model for 66 members in Two Dimensions, four ITEM anchors WITH CUTLINE ESTIMATES
######################################################################
###cat("
###model {
###  for (j in 1:J){                ## loop over diputados
###    for (i in 1:I){              ## loop over items
###      #y.hat[j,i] ~ dbern(p[j,i]);                                  ## voting rule
###      #p[j,i] <- phi(v.star[j,i]);                                  ## sets 0<p<1
###      v.star[j,i] ~ dnorm(mu[j,i],1)I(lo.v[j,i],hi.v[j,i]);   ## truncated normal sampling
###      mu[j,i] <- delta[i]*a[i]*x[j] + delta[i]*b[i] - delta[i]*y[j] ## utility differential
###                  }
###                }
###  for (i in 1:I){
###  a[i] <- sin(angle[i]) / sqrt(1-sin(angle[i])*sin(angle[i]))
###  }
###  ## priors ################
###for (j in 1:J){
###    x[j] ~  dnorm(0, 1)
###    y[j] ~  dnorm(0, 1)
###    }
###for(i in 1:(V1-1)){
###    delta[i] ~ dnorm( 0, 0.25)
####    angle[i] ~ dunif(-1.57,1.57) # (-pi/2,pi/2)
###    angle[i] ~ dunif(.392,1.178) # (pi/8,3pi/8)
###    b[i] ~ dnorm( 0, .25)
###    }
###delta[V1] ~ dnorm( -4, 4)
###angle[V1] ~ dunif(1.37,1.77) # (7pi/16,9pi/16) ---- VERTICAL
###b[V1] ~ dnorm( 0, 4)
###for(i in (V1+1):(H1-1)){
###    delta[i] ~ dnorm( 0, 0.25)
####    angle[i] ~ dunif(-1.57,1.57) # (-pi/2,pi/2)
###    angle[i] ~ dunif(.392,1.178) # (pi/8,3pi/8)
###    b[i] ~ dnorm( 0, .25)
###    }
###delta[H1] ~ dnorm( -4, 4)
###angle[H1] ~ dunif(-0.2,0.2) # (-pi/16,pi/16) ------ HORIZONTAL
###b[H1] ~ dnorm( 0, 4)
###for(i in (H1+1):(V2-1)){
###    delta[i] ~ dnorm( 0, 0.25)
####    angle[i] ~ dunif(-1.57,1.57) # (-pi/2,pi/2)
###    angle[i] ~ dunif(.392,1.178) # (pi/8,3pi/8)
###    b[i] ~ dnorm( 0, .25)
###    }
###delta[V2] ~ dnorm( 4, 4)
###angle[V2] ~ dunif(1.37,1.77) # (7pi/16,9pi/16) ---- VERTICAL
###b[V2] ~ dnorm( 0, 4)
###for(i in (V2+1):(H2-1)){
###    delta[i] ~ dnorm( 0, 0.25)
####    angle[i] ~ dunif(-1.57,1.57) # (-pi/2,pi/2)
###    angle[i] ~ dunif(.392,1.178) # (pi/8,3pi/8)
###    b[i] ~ dnorm( 0, .25)
###    }
###delta[H2] ~ dnorm( -4, 4)
###angle[H2] ~ dunif(-0.2,0.2) # (-pi/16,pi/16) ------ HORIZONTAL
###b[H2] ~ dnorm( 0, 4)
###for(i in (H2+1):I){
###    delta[i] ~ dnorm( 0, 0.25)
####    angle[i] ~ dunif(-1.57,1.57) # (-pi/2,pi/2)
###    angle[i] ~ dunif(.392,1.178) # (pi/8,3pi/8)
###    b[i] ~ dnorm( 0, .25)
###    }
###}
###", file="model66Sta2Di4.txt")
####
######################################################################
###### static model for 66 members in Two Dimensions, two ITEM anchors WITH CUTLINE ESTIMATES
######################################################################
###cat("
###model {
###  for (j in 1:J){                ## loop over diputados
###    for (i in 1:I){              ## loop over items
###      #y.hat[j,i] ~ dbern(p[j,i]);                                  ## voting rule
###      #p[j,i] <- phi(v.star[j,i]);                                  ## sets 0<p<1
###      v.star[j,i] ~ dnorm(mu[j,i],1)I(lo.v[j,i],hi.v[j,i]);   ## truncated normal sampling
###      mu[j,i] <- delta[i]*a[i]*x[j] + delta[i]*b[i] - delta[i]*y[j] ## utility differential
###                  }
###                }
###  for (i in 1:I){
###  a[i] <- sin(angle[i]) / sqrt(1-sin(angle[i])*sin(angle[i]))
###  }
###  ## priors ################
###for (j in 1:J){
###    x[j] ~  dnorm(0, 1)
###    y[j] ~  dnorm(0, 1)
###    }
###for(i in 1:(V1-1)){
###    delta[i] ~ dnorm( 0, 0.25)
####    angle[i] ~ dunif(-1.57,1.57) # (-pi/2,pi/2)
###    angle[i] ~ dunif(.392,1.178) # (pi/8,3pi/8)
###    b[i] ~ dnorm( 0, .25)
###    }
###delta[V1] ~ dnorm( -4, 4)
###angle[V1] ~ dunif(1.37,1.77) # (7pi/16,9pi/16) ---- VERTICAL
###b[V1] ~ dnorm( 0, 4)
###for(i in (V1+1):(H1-1)){
###    delta[i] ~ dnorm( 0, 0.25)
####    angle[i] ~ dunif(-1.57,1.57) # (-pi/2,pi/2)
###    angle[i] ~ dunif(.392,1.178) # (pi/8,3pi/8)
###    b[i] ~ dnorm( 0, .25)
###    }
###delta[H1] ~ dnorm( -4, 4)
###angle[H1] ~ dunif(-0.2,0.2) # (-pi/16,pi/16) ------ HORIZONTAL
###b[H1] ~ dnorm( 0, 4)
###for(i in (H1+1):I){
###    delta[i] ~ dnorm( 0, 0.25)
####    angle[i] ~ dunif(-1.57,1.57) # (-pi/2,pi/2)
###    angle[i] ~ dunif(.392,1.178) # (pi/8,3pi/8)
###    b[i] ~ dnorm( 0, .25)
###    }
###}
###", file="model66Sta2Di2.txt")
####
###### dynamic model for 66 members in Two Dimensions -- IRT PARAMETERIZATION
###cat("
###model {
###  for (j in 1:J){                ## loop over diputados
###    for (i in 1:I){              ## loop over items
###      #v.hat[j,i] ~ dbern(p[j,i]);                                  ## voting rule
###      #p[j,i] <- phi(v.star[j,i]);                                  ## sets 0<p<1
###      v.star[j,i] ~ dnorm(mu[j,i],1)I(lo.v[j,i],hi.v[j,i]);   ## truncated normal sampling
###      mu[j,i] <- beta[i]*(xOne[j]*d1[i] + xTwo[j]*d2[i] + xThree[j]*d3[i] + xFour[j]*d4[i] + xFive[j]*d5[i] + xSix[j]*d6[i] + xSeven[j]*d7[i] + xEight[j]*d8[i])
###                - alpha[i] + delta[i]*(yOne[j]*d1[i] + yTwo[j]*d2[i] + yThree[j]*d3[i] + yFour[j]*d4[i] + yFive[j]*d5[i] + ySix[j]*d6[i] + ySeven[j]*d7[i] + yEight[j]*d8[i])  ## utility differential
###                  }
###      xOne[j] ~   dnorm (xZero[j],15);  ## en 2do intento slack era 20, 3ro 10
###      xTwo[j] ~   dnorm (xOne[j],15);
###      xThree[j] ~ dnorm (xTwo[j],15);
###      xFour[j] ~  dnorm (xThree[j],15);
###      xFive[j] ~  dnorm (xFour[j],15);
###      xSix[j] ~   dnorm (xFive[j],15);
###      xSeven[j] ~ dnorm (xSix[j],15);
###      xEight[j] ~ dnorm (xSeven[j],15);
###      yOne[j] ~   dnorm (yZero[j],15);
###      yTwo[j] ~   dnorm (yOne[j],15);
###      yThree[j] ~ dnorm (yTwo[j],15);
###      yFour[j] ~  dnorm (yThree[j],15);
###      yFive[j] ~  dnorm (yFour[j],15);
###      ySix[j] ~   dnorm (yFive[j],15);
###      ySeven[j] ~ dnorm (ySix[j],15);
###      yEight[j] ~ dnorm (ySeven[j],15);
###                }
##### ESTO LO PUEDO SACAR POST ESTIMACION
#####  for (i in 1:I){
#####  a[i] <- beta[i] / delta[i]  ## pendiente de cutline
#####  b[i] <- alpha[i] / delta[i] ## constante de cutline
#####  }
###  ################
###  ## priors
###  ################
##### 1a dim ############
###for (j in 1:(N-1)){
###    xZero[j] ~  dnorm(0, 1)
###    }
###    xZero[N] ~  dnorm(0, 4)    # Mrs. NORTH Piña Olmedo Laura (PRD)
####    xZero[N] <- 0
###for (j in (N+1):(W-1)){
###    xZero[j] ~  dnorm(0, 1)
###    }
###    xZero[W] ~  dnorm(-2, 4)    # Mr. WEST Méndez Rangel Avelino (PRD)
####    xZero[W] <- -2
###for (j in (W+1):(E-1)){
###    xZero[j] ~  dnorm(0, 1)
###    }
###    xZero[E] ~  dnorm(2, 4)    # Mrs. EAST Paula Adriana Soto Maldonado (PAN)
####    xZero[E] <- 2
###for (j in (E+1):(S-1)){
###    xZero[j] ~  dnorm(0, 1)
###    }
###    xZero[S] ~  dnorm(0, 4)    # Mr. SOUTH Tenorio Antiga Xiuh (PANAL)
####    xZero[S] <- 0
###for (j in (S+1):J){
###    xZero[j] ~  dnorm(0, 1)
###    }
##### 2a dim  ############
###for (j in 1:(N-1)){
###    yZero[j] ~  dnorm(0, 1)
###    }
###    yZero[N] ~  dnorm(2, 4)    # Mrs. NORTH
####    yZero[N] <- 2
###for (j in (N+1):(W-1)){
###    yZero[j] ~  dnorm(0, 1)
###    }
###    yZero[W] ~  dnorm(0, 4)    # Mr. WEST
####    yZero[W] <- 0
###for (j in (W+1):(E-1)){
###    yZero[j] ~  dnorm(0, 1)
###    }
###    yZero[E] ~  dnorm(0, 4)    # Mrs. EAST
####    yZero[E] <- 0
###for (j in (E+1):(S-1)){
###    yZero[j] ~  dnorm(0, 1)
###    }
###    yZero[S] ~  dnorm(-2, 4)    # Mr. SOUTH
####    yZero[S] <- -2
###for (j in (S+1):J){
###    yZero[j] ~  dnorm(0, 1)
###    }
###    for(i in 1:I){
###        alpha[i] ~ dnorm( 0, 1)
###        beta[i]  ~ dnorm( 0, 1)
###        delta[i] ~ dnorm( 0, 1)
###                 }
###}
###", file="model66Dyn2Dj.irt.txt")
####
####
##########################################################################################
###### dynamic model for 66 members in Two Dimensions Four Item anchors-- IRT PARAMETERIZATION
##########################################################################################
###cat("
###model {
###  for (j in 1:J){                ## loop over diputados
###    for (i in 1:I){              ## loop over items
###      #v.hat[j,i] ~ dbern(p[j,i]);                                  ## voting rule
###      #p[j,i] <- phi(v.star[j,i]);                                  ## sets 0<p<1
###      v.star[j,i] ~ dnorm(mu[j,i],1)I(lo.v[j,i],hi.v[j,i]);   ## truncated normal sampling
###      mu[j,i] <- beta[i]*(xOne[j]*d1[i] + xTwo[j]*d2[i] + xThree[j]*d3[i]
###                 + xFour[j]*d4[i] + xFive[j]*d5[i] + xSix[j]*d6[i] + xSeven[j]*d7[i]
###                 + xEight[j]*d8[i])
###                 - alpha[i] + delta[i]*(yOne[j]*d1[i] + yTwo[j]*d2[i] + yThree[j]*d3[i]
###                 + yFour[j]*d4[i] + yFive[j]*d5[i] + ySix[j]*d6[i] + ySeven[j]*d7[i]
###                 + yEight[j]*d8[i])  ## utility differential
###                  }
###      xOne[j] ~   dnorm (xZero[j],15);  ## en 2do intento slack era 20, 3ro 10
###      xTwo[j] ~   dnorm (xOne[j],15);
###      xThree[j] ~ dnorm (xTwo[j],15);
###      xFour[j] ~  dnorm (xThree[j],15);
###      xFive[j] ~  dnorm (xFour[j],15);
###      xSix[j] ~   dnorm (xFive[j],15);
###      xSeven[j] ~ dnorm (xSix[j],15);
###      xEight[j] ~ dnorm (xSeven[j],15);
###      yOne[j] ~   dnorm (yZero[j],15);
###      yTwo[j] ~   dnorm (yOne[j],15);
###      yThree[j] ~ dnorm (yTwo[j],15);
###      yFour[j] ~  dnorm (yThree[j],15);
###      yFive[j] ~  dnorm (yFour[j],15);
###      ySix[j] ~   dnorm (yFive[j],15);
###      ySeven[j] ~ dnorm (ySix[j],15);
###      yEight[j] ~ dnorm (ySeven[j],15);
###                }
##### ESTO LO PUEDO SACAR POST ESTIMACION
#####  for (i in 1:I){
#####  a[i] <- beta[i] / delta[i]  ## pendiente de cutline
#####  b[i] <- alpha[i] / delta[i] ## constante de cutline
#####  }
###  ################
###  ## priors
###  ################
##### 1a dim ############
###for (j in 1:(N-1)){
###    xZero[j] ~  dnorm(0, 1)
###    yZero[j] ~  dnorm(0, 1)
###    }
###    xZero[N] ~  dnorm(-2, 4)    # Mrs. NORTH Piña Olmedo Laura (PRD)
###    yZero[N] ~  dnorm(2, 4)
####    xZero[N] <- 0
####    yZero[N] <- 2
###for (j in (N+1):J){
###    xZero[j] ~  dnorm(0, 1)
###    yZero[j] ~  dnorm(0, 1)
###    }
###for(i in 1:(V1-1)){
###    alpha[i] ~ dnorm( 0, 1)
###    beta[i]  ~ dnorm( 0, 1)
###    delta[i] ~ dnorm( 0, 1)
###    }
###alpha[V1] ~ dnorm( 0, 1)
###beta[V1]  ~ dnorm(-4, 20)
###delta[V1] ~ dnorm(-4, 20)
###for(i in (V1+1):(H1-1)){
###    alpha[i] ~ dnorm( 0, 1)
###    beta[i]  ~ dnorm( 0, 1)
###    delta[i] ~ dnorm( 0, 1)
###    }
###alpha[H1] ~ dnorm( 0, 1)
###beta[H1]  ~ dnorm( 4, 20)
###delta[H1] ~ dnorm(-4, 20)
###for(i in (H1+1):(V2-1)){
###    alpha[i] ~ dnorm( 0, 1)
###    beta[i]  ~ dnorm( 0, 1)
###    delta[i] ~ dnorm( 0, 1)
###    }
###alpha[V2] ~ dnorm( 0, 1)
###beta[V2]  ~ dnorm( 4, 20)
###delta[V2] ~ dnorm( 4, 20)
###for(i in (V2+1):(H2-1)){
###    alpha[i] ~ dnorm( 0, 1)
###    beta[i]  ~ dnorm( 0, 1)
###    delta[i] ~ dnorm( 0, 1)
###    }
###alpha[H2] ~ dnorm( 0, 1)
###beta[H2]  ~ dnorm( 4, 20)
###delta[H2] ~ dnorm(-4, 20)
###for(i in (H2+1):I){
###    alpha[i] ~ dnorm( 0, 1)
###    beta[i]  ~ dnorm( 0, 1)
###    delta[i] ~ dnorm( 0, 1)
###    }
###}
###", file="model66Dyn2Di4.irt.txt")
####
############################################################################################

## JAGS VERSION
v <- t(rc)
J <- nrow(v); I <- ncol(v)
ip.data <- list ("J", "I", "v", "PAN", "PRI", "PRD", "PVEM", "CONVE")
ip.inits <- function (){
    list (
#    v=rbern(J*I),
    delta=rnorm(I),
    alpha=rnorm(I),
    beta=rnorm(I),
    x=rnorm(J),
    y=rnorm(J)
    )
    }
ip.parameters <- c("delta","beta", "alpha", "x", "y")#, "deviance")

## ## BUGS VERSION
## v <- rc
## v <- t(v)
## J <- nrow(v); I <- ncol(v)
## lo.v <- ifelse(is.na(v)==TRUE | v== 1, 0, -5)
## hi.v <- ifelse(is.na(v)==TRUE | v==-1,  0, 5)
## vstar <- matrix (NA, nrow=J, ncol=I)
## for (j in 1:J){
## for (i in 1:I){
##   vstar[j,i] <- ifelse(v[j,i]==0, 0, ifelse(v[j,i]==1, runif(1), -1*runif(1)))}}
## rm(v)
## ip.data <- list ("J", "I", "lo.v", "hi.v", "PAN", "PRI", "PRD", "PVEM", "CONVE")
## ip.inits <- function (){
##     list (
##     v.star=vstar,
##     delta=rnorm(I),
##     alpha=rnorm(I),
##     beta=rnorm(I),
##     x=rnorm(J),
##     y=rnorm(J)
##     )
##     }
## ip.parameters <- c("delta","beta", "alpha", "x", "y") #, "deviance")

#test ride to see program works
start.time <- proc.time()
results <- jags (ip.data, ip.inits, ip.parameters,
                "model2Dj.irt.txt", n.chains=2,
                n.iter=10, n.thin=1#, debug=T,
#                bugs.seed = ch*10000,
#                bugs.directory = "C:/Program Files (x86)/WinBUGS14",
#                bugs.directory = "c:/Archivos de programa/WinBUGS14/",
#                bugs.directory = "~/.wine/dosdevices/c:/Program Files (x86)/WinBUGS14",
#                program = c("WinBUGS")
                 )
time.elapsed <- round(((proc.time()-start.time)[3])/60/60,2); rm(start.time)
print(cat("\tTime elapsed in estimation:",time.elapsed,"hours","\n")); rm(time.elapsed)

#longer run
start.time <- proc.time()
results <- jags (ip.data, ip.inits, ip.parameters,
                "model2Dj.irt.txt", n.chains=2,
                n.iter=6000, n.burnin=4000, n.thin=20#, debug=F,
#                bugs.seed = ch*10000,
#                bugs.directory = "c:/Program Files (x86)/WinBUGS14/",
#                bugs.directory = "c:/Archivos de programa/WinBUGS14/",
#                program = c("WinBUGS")
                )
time.elapsed <- round(((proc.time()-start.time)[3])/60/60,2); rm(start.time)
print(cat("\tTime elapsed in estimation:",time.elapsed,"hours","\n")); rm(time.elapsed)


plot(results)
print(results)

#to continue running
tmp1<-list (
    v.star=vstar,
    delta=results$last.values[[1]]$delta,
    alpha=results$last.values[[1]]$alpha,
    beta=results$last.values[[1]]$beta,
    x=results$last.values[[1]]$x,
    y=results$last.values[[1]]$y
    )
tmp2<-list (
    v.star=vstar,
    delta=results.2$last.values[[1]]$delta,
    alpha=results.2$last.values[[1]]$alpha,
    beta=results.2$last.values[[1]]$beta,
    x=results.2$last.values[[1]]$x,
    y=results.2$last.values[[1]]$y
    )
tmp3<-list (
    v.star=vstar,
    delta=results.3$last.values[[1]]$delta,
    alpha=results.3$last.values[[1]]$alpha,
    beta=results.3$last.values[[1]]$beta,
    x=results.3$last.values[[1]]$x,
    y=results.3$last.values[[1]]$y
    )
### for (chain in 1:3){dip.61$last.values[[chain]]$v.star <- vstar}
results.bis <- bugs (ip.data,
                inits=list( tmp1,tmp2,tmp3 ),
                ip.parameters,
                "model2Dj.irt.txt", n.chains=2,
                n.iter=10000, n.burnin=7500, n.thin=25, debug=F,
                bugs.seed = ch*10000,
                bugs.directory = "c:/Program Files (x86)/WinBUGS14/",
#                bugs.directory = "c:/Archivos de programa/WinBUGS14/",
                program = c("WinBUGS"))

## change .ch number appropriately for each chain
results.2 <- results
save(results.2, file="tmp2.RData")
#rm(results)

## COMBINE UNICHAINS RUN SEPARATELY: LOAD EACH CHAIN NAMED results.ch, THEN:
load("tmp2.RData"); load("tmp3.RData")
## results <- results.1 ## so that it inherits mcmc.list attribute
## results[[2]] <- results.2[[1]]
## results[[3]] <- results.3[[1]]
results <- list(results, results.2, results.3)
rm(results.2, results.3)
chains <- list ( results[[1]]$sims.matrix, results[[2]]$sims.matrix, results[[3]]$sims.matrix )

## PLOT CHAINS TO CHECK CONVERGENCE
tmp <- dimnames(results[[1]]$sims.matrix); tmp <- tmp[[2]]
cplt <- function(X)
    {
    tmp2 <- min(as.numeric(chains[[1]][,X]), as.numeric(chains[[2]][,X]), as.numeric(chains[[3]][,X]));
#    tmp2 <- min(as.numeric(results$mcmc[[1]][,X]), as.numeric(results[[2]][,X]), as.numeric(results[[3]][,X]));
    tmp3 <- max(as.numeric(chains[[1]][,X]), as.numeric(chains[[2]][,X]), as.numeric(chains[[3]][,X]));
    plot(c(1,results[[1]]$n.sims), c(tmp2,tmp3), type="n", main=tmp[X]);
    lines(1:(results[[1]]$n.sims), as.numeric(chains[[1]][,X]),col="red");
    lines(1:(results[[1]]$n.sims), as.numeric(chains[[2]][,X]),col="blue");
    lines(1:(results[[1]]$n.sims), as.numeric(chains[[3]][,X]),col="green");
    }
##
setwd(paste(workdir, "/graphs/convergence", sep=""))
for (i in 1:length(chains[[1]][1,]))
    {
    pdf( paste("tmp", i, ".pdf", sep=""))
    cplt(i)
    dev.off()
    }
setwd(workdir)

## post.draw <- rbind(chains[[1]], chains[[2]], chains[[3]])
## post.x     <- post.draw[,grep("x", colnames(post.draw))]
## post.y     <- post.draw[,grep("y", colnames(post.draw))]
## post.alpha <- post.draw[,grep("alpha", colnames(post.draw))]
## post.beta  <- post.draw[,grep("beta", colnames(post.draw))]
## post.delta <- post.draw[,grep("delta", colnames(post.draw))]

load(file = "ip60_5+5k.RData")
post.draw <- results$sims.matrix
post.x     <- post.draw[,grep("x", colnames(post.draw))]
post.y     <- post.draw[,grep("y", colnames(post.draw))]
post.alpha <- post.draw[,grep("alpha", colnames(post.draw))]
post.beta  <- post.draw[,grep("beta", colnames(post.draw))]
post.delta <- post.draw[,grep("delta", colnames(post.draw))]

## SACA CONSTANTE Y PENDIENTE DE CUTLINES
a <- post.beta / post.delta  ## pendiente de cutline
b <- post.alpha / post.delta ## constante de cutline

## 45-DEGREE CLOCKWISE ROTATION OF COORDINATES (SO THAT PRIORS REMAIN n, s, e, w)
xR <- post.x*cos(pi/4) + post.y*sin(pi/4)
yR <- -post.x*sin(pi/4) + post.y*cos(pi/4)
post.x <- xR; post.y <- yR;
## 45-DEGREE CLOCKWISE ROTATION OF CUTLINES
xA <- -b/a; yA <- rep(0, length(a)); xO <- rep(0, length(a)); yO <- b  ## coords de Abscisa al origen y Ordenada al origan de c/cutline
xAR <- xA*cos(pi/4) + yA*sin(pi/4)
yAR <- -xA*sin(pi/4) + yA*cos(pi/4)
xOR <- xO*cos(pi/4) + yO*sin(pi/4)
yOR <- -xO*sin(pi/4) + yO*cos(pi/4)
X <- xAR; Y <- yAR; XX <- xOR; YY <- yOR ## simplifica notación
aR <- (YY-Y)/(XX-X)           ## pendiente del cutline rotado
bR <- YY -((YY-Y)/(XX-X))*XX  ## constante del cutline rotado
rm(X,Y,XX,YY)
a <- aR; b <- bR

jotas <- matrix(NA, nrow=J, ncol=6)
for (j in 1:J){
    jotas[j,1] <- quantile (post.x[,j], 0.025, names=F)
    jotas[j,2] <- quantile (post.x[,j], 0.50, names=F)
    jotas[j,3] <- quantile (post.x[,j], 0.975, names=F)
    jotas[j,4] <- quantile (post.y[,j], 0.025, names=F)
    jotas[j,5] <- quantile (post.y[,j], 0.50, names=F)
    jotas[j,6] <- quantile (post.y[,j], 0.975, names=F)
    }

dipdat$symbol <- dipdat$color;
dipdat$symbol[dipdat$symbol=="darkblue"] <- "A"
dipdat$symbol[dipdat$symbol=="forestgreen"] <- "R"
dipdat$symbol[dipdat$symbol=="gold"] <- "D"
dipdat$symbol[dipdat$symbol=="red"] <- "T"
dipdat$symbol[dipdat$symbol=="darkolivegreen2"] <- "V"
dipdat$symbol[dipdat$symbol=="orange"] <- "C"
dipdat$symbol[dipdat$symbol=="cyan"] <- "L"
dipdat$symbol[dipdat$symbol=="darkorchid"] <- "S"
#
sym.list <- c("A", "R", "D" , "T", "V", "C", "L", "S")

## dipdat$color[dipdat$color=="."] <- "black"
##plot(c(min(jotas[,2]),max(jotas[,2])), c(min(jotas[,5]),max(jotas[,5])), type="n")
plot(c(-2,2), c(-2,2), type="n",
           xlab="",
           ylab="",
#           xlab=c("dim2_all"),
#           ylab=c("dim2_gaceta"),
#           main="60th Leg. 2006-09")
           main="60a Leg. 2006-09")
##abline(0,1)
## points(jotas[,2],jotas[,5], pch=19, col=dipdat$color)
## legend(1.45,2.1, legend=part.list, cex=1, pch=20, pt.cex=1.25, col=color.list, bg="white")
points(jotas[,2],jotas[,5], pch=dipdat$symbol)#, col=dipdat$color)
legend(1.3,2.1, legend=paste(sym.list, "=", part.list), cex=1, pch=20, pt.cex=1.25, col="white", bg="white")
##points(jotas[,5],jotas.gac[,5], pch=19, cex=.75, col=dipdat$color)


## cutlines
amed <- rep(NA,times=I)
bmed <- rep(NA,times=I)
for (i in 1:I){
    amed[i] <- quantile (a[,i], 0.50, names=F)
    bmed[i] <- quantile (b[,i], 0.50, names=F)  }
#
plot(c(-2.2,2.2), c(-2.2,2.2), type="n",
           xlab=c(""),
           ylab=c(""),
           main="60th Leg. 2006-09")
    for (i in 1:I){
        abline(a=bmed[i], b=amed[i], col="grey") } ## OJO: a en mi modelo es slope, en R es constant
    for (j in 1:J){
        points(jotas[j,2],jotas[j,5],pch=20,col=dipdat$color[j])
        }


### Exporta coordenadas de todos los diputados
tmp <- matrix(NA, nrow=67, ncol=4)
tmp[,1] <- as.numeric(jotas[,2])
tmp[,2] <- jotas[,5]
tmp[,3] <- names.67
tmp[,4] <- part.67
tmp<-data.matrix(tmp)
tmp[,1:2] <- as.numeric(tmp[,1:2])
write.table(tmp, file="aldfStaticIdPts.xls", sep=",")



# For use in rollrates analysis
postNorthprd <- rep(0,67)
postSouthprd <- rep(0,67)
for (j in 1:67){
    postNorthprd[j] <- ifelse (jotas[j,5]>=0 & part.67[j]=="prd", 1, 0)
    postSouthprd[j] <- ifelse (jotas[j,5]<0 & part.67[j]=="prd", 1, 0)
    }

amed <- rep(NA,times=I)
bmed <- rep(NA,times=I)
for (i in 1:I){
    amed[i] <- quantile (a[,i], 0.50, names=F)
    bmed[i] <- quantile (b[,i], 0.50, names=F)  }

### Exporta coordenadas de todos los diputados
tmp <- matrix(NA, nrow=67, ncol=4)
tmp[,1] <- as.numeric(jotas[,2])
tmp[,2] <- jotas[,5]
tmp[,3] <- names.67
tmp[,4] <- part.67
tmp<-data.matrix(tmp)
tmp[,1:2] <- as.numeric(tmp[,1:2])
write.table(tmp, file="aldfStaticIdPts.xls", sep=",")


### TO RESET GRAPH PARAMETERS SAY par(oldpar) ###
oldpar <- par(no.readonly=TRUE)

#par(mfrow=c(3,3))
#par("pin" = c(.63,.58)) #width and height of plot region in inches

## FUNCTION TO DRAW ELLIPSES OVOIDS
ellipsePoints <- function(a,b, alpha = 0, loc = c(0,0), n = 201)
{
    ## Purpose: ellipse points,radially equispaced, given geometric par.s
    ## -------------------------------------------------------------------------
    ## Arguments: a, b : length of half axes in (x,y) direction
    ##            alpha: angle (in degrees) for rotation
    ##            loc  : center of ellipse
    ##            n    : number of points
    ## -------------------------------------------------------------------------
    ## Author: Martin Maechler, Date: 19 Mar 2002, 16:26
    B <- min(a,b)
    A <- max(a,b)
    ## B <= A
    d2 <- (A-B)*(A+B)                   #= A^2 - B^2
    phi <- 2*pi*seq(0,1, len = n)
    sp <- sin(phi)
    cp <- cos(phi)
    r <- a*b / sqrt(B^2 + d2 * sp^2)
    xy <- r * cbind(cp, sp)
    ## xy are the ellipse points for alpha = 0 and loc = (0,0)
    al <- alpha * pi/180
    ca <- cos(al)
    sa <- sin(al)
    xy %*% rbind(c(ca, sa), c(-sa, ca)) + cbind(rep(loc[1],n),
                                                rep(loc[2],n))
}

tmp <- c(jotas[,1],jotas[,4])
for (i in 1:length(tmp)) { tmp[i] <- ifelse(is.na(tmp[i])==TRUE,0,tmp[i]) }
min <- min( tmp )
tmp <- c(jotas[,3],jotas[,6])
for (i in 1:length(tmp)) { tmp[i] <- ifelse(is.na(tmp[i])==TRUE,0,tmp[i]) }
max <- max( tmp )
lims <- c(NA,NA)
lims[1] <- ifelse(abs(min)>max, min, -max)
lims[2] <- ifelse(abs(min)>max, abs(min), max)

#### FOR USE IF ELLIPSES WILL BE GRAPHED
##eps <- array(NA, dim=c(201,2,67))
##eps[,,1] <- ellipsePoints(a=jotas[t,2,1]-jotas[t,1,1],b=jotas[t,5,1]-jotas[t,4,1],alpha=0,loc=c(jotas[t,2,1],jotas[t,5,1]),n=201)
##eps[,,2] <- ellipsePoints(a=jotas[t,2,2]-jotas[t,1,2],b=jotas[t,5,2]-jotas[t,4,2],alpha=0,loc=c(jotas[t,2,2],jotas[t,5,2]),n=201)
##etcetera

par(mar = c(3.1, 3.1, 2.1, 2.1) )
plot(c(-1.5,1.5),c(-1.5,1.5),type="n",
       xlab=c(""), ##xlab=c("pro-SQ                                         pro-change"),
       ylab=c(""), ##ylab=c("interpretivist                                            literalist"),
       main=c("")) ##main=paste("Acc+Con (ancla j) time=",t,I,"obs"))
abline(-1.5,0,col="grey",lty=3); abline(-1,0,col="grey",lty=3); abline(-.5,0,col="grey",lty=3); abline(0,0,col="grey",lty=3); abline(.5,0,col="grey",lty=3); abline(1,0,col="grey",lty=3); abline(1.5,0,col="grey",lty=3);
abline(v=-1.5,col="grey",lty=3); abline(v=-1,col="grey",lty=3); abline(v=-.5,col="grey",lty=3); abline(v=0,col="grey",lty=3); abline(v=.5,col="grey",lty=3); abline(v=1,col="grey",lty=3); abline(v=1.5,col="grey",lty=3);
legend(1.1,-.65, legend=part.list, cex=.75, pch=20, pt.cex=1.25, col=color.list, bg="white")
##for (j in 1:J){
##    segments(jotas[t,1,j],jotas[t,5,j],jotas[t,3,j],jotas[t,5,j],col="gray")
##    segments(jotas[t,2,j],jotas[t,4,j],jotas[t,2,j],jotas[t,6,j],col="gray")
##    }
##for (j in 1:J){
##    lines(eps[,,j],col=color.67[j])
##    }
for (j in 1:J){
    points(jotas[j,2],jotas[j,5],pch=20,col=color.67[j])
    }
for (j in 1:J){
    points(jotas[j,2],jotas[j,5], col=dCoord[j]); ## pone coordinadores
    }
##for (j in 1:J){
##    text(jotas[j,2],jotas[j,5],labels=coords[j])
##    }


## cutlines
#cuad <- 1*d1+2*d2+3*d3+4*d4+5*d5+6*d6+7*d7+8*d8 ## could be handy to draw cutlines for some t in static map
    plot(c(-1.5,1.5),c(-1.5,1.5),type="n",
           xlab=c(""),
           ylab=c(""),
           main=c(""))
#    atmp <- amed[cuad==t]; btmp <- bmed[cuad==t];
    atmp <- amed; btmp <- bmed;
    N <- length(atmp)
    for (n in 1:N){
        abline(a=btmp[n], b=atmp[n], col="grey") } ## OJO: a en mi modelo es slope, en R es constant
    for (j in 1:J){
        points(jotas[j,2],jotas[j,5],pch=20,col=color.67[j])
        }

## cutlines one-by-one
setwd("d:/01/data/rollcall/aldf/graphs/cutlinesOnebyOne")
    atmp <- amed; btmp <- bmed;
    N <- length(atmp)
#    n <- 12
    for (n in 1:N){
        plot(c(-2,2),c(-2,2),type="n",
           xlab=c(""),
           ylab=c(""),
           main=paste(RCs$yr[n],"-",RCs$mo[n],"-",RCs$dy[n],"#",RCs$folio[n],"  (",RCs$favor[n],"/",RCs$contra[n],"/",RCs$absten[n],")",sep=""))
        abline(a=btmp[n], b=atmp[n], col="black")
            for (j in 1:J){
                points(jotas[j,2],jotas[j,5],pch=20,col=color.67[j])
                }
        savePlot(filename = paste("cutline",n, sep=""), type = "pdf")
    }
setwd("d:/01/data/rollcall/aldf")





#########################################################################
#########################################################################
###   Static 2Dimensions three item anchors, irt paremeterization     ###
#########################################################################
#########################################################################

## PARTY VOTE BREAKDOWNS TO INSPECT VOTES
I <- dim(rc)[1];
tmp <- data.frame(ayes=rep(NA, I), nays=rep(NA, I), valid=rep(NA, I), abs=rep(NA, I), tot=rep(NA, I))
vot.pan <- tmp; vot.pri <- tmp; vot.prd <- tmp; vot.pt  <- tmp; rm(tmp)
for (i in 1:I){
   vot.pan[i,] <- count.votes(rc[i,dipdat$part=="pan"])
   vot.pri[i,] <- count.votes(rc[i,dipdat$part=="pri"])
   vot.prd[i,] <- count.votes(rc[i,dipdat$part=="prd"])
   vot.pt[i,]  <- count.votes(rc[i,dipdat$part=="pt"])
                     }
   vot.pan[,1] <- round(vot.pan[,1]/vot.pan[,3], 2)
   vot.pan[,2] <- round(vot.pan[,2]/vot.pan[,3], 2)
   vot.pan[,4] <- round(vot.pan[,4]/vot.pan[,5], 2)
   vot.pri[,1] <- round(vot.pri[,1]/vot.pri[,3], 2)
   vot.pri[,2] <- round(vot.pri[,2]/vot.pri[,3], 2)
   vot.pri[,4] <- round(vot.pri[,4]/vot.pri[,5], 2)
   vot.prd[,1] <- round(vot.prd[,1]/vot.prd[,3], 2)
   vot.prd[,2] <- round(vot.prd[,2]/vot.prd[,3], 2)
   vot.prd[,4] <- round(vot.prd[,4]/vot.prd[,5], 2)
   vot.pt[,1] <- round(vot.pt[,1]/vot.pt[,3], 2)
   vot.pt[,2] <- round(vot.pt[,2]/vot.pt[,3], 2)
   vot.pt[,4] <- round(vot.pt[,4]/vot.pt[,5], 2)

## POTENTIAL ANCHORS (FOLIO):
## 1401 Amend ISSSTE reform to keep pensions in Govt hands (SQ) PRI+PAN nay PRD aye --> + = left
## 1477 Impuesto a dep?sitos en efectivo PRI+PAN aye PRD nay --> + = right
## 1565 COFIPE PAN+PRI+PRD aye AMLO nay --> + = north
#tmp <- ifelse( votdat$folio==1401 | votdat$folio==1477 | votdat$folio==1565, 1, 0)
#data.frame(panA=vot.pan$ayes[tmp==1], prdA=vot.prd$ayes[tmp==1], priA=vot.pri$ayes[tmp==1], folio=votdat$folio[tmp==1], tit=votdat$title[tmp==1])
##
west  <- grep (votdat$folio, pattern="\\<1401\\>")
east  <- grep (votdat$folio, pattern="\\<1477\\>")
north <- grep (votdat$folio, pattern="\\<1565\\>")

etc


#######################################################
#######################################################
###      Static 1Dimension two extremist anchors    ###
#######################################################
#######################################################

## ANCHORS
##          AgsPan                      Noroña
#DER<-grep("ags01p", dip$id); IZQ<-grep("df19p", dip$id)
##          Luken                      Cárdenas
DER<-grep("bc05p", dip$id); IZQ<-grep("df04p", dip$id)

### ALLOWS TO DROP CASES FROM ANALYSIS
#year1 <- ifelse( (cuad==1 | cuad==2 | cuad==3), 1, 0 ) ## FALTA QUITAR A UN DIPUTADO (MÁS?) QUE NO ENTRÓ HASTA DESPUÉS
#drop <- ifelse(year1==0,1,0)
#tmp<-rc[drop==0,]
#rc<-tmp
#sem<-sem[drop==0]; cuad<-cuad[drop==0]; trim<-trim[drop==0]

votes <- rc
## TAKE SAMPLE HERE IF SO WISHED
rnd <- runif(dim(rc)[1]); votes <- votes[rnd<.3,]
votes <- t(votes)
J <- nrow(votes); I <- ncol(votes)
v <- votes
lo.v <- ifelse(is.na(v)==TRUE | v== 1, 0, -50)
hi.v <- ifelse(is.na(v)==TRUE | v==-1,  0, 50)
vstar <- matrix (NA, nrow=J, ncol=I)
for (j in 1:J){
for (i in 1:I){
  vstar[j,i] <- ifelse(v[j,i]==0, 0, ifelse(v[j,i]==1, runif(1), -1*runif(1)))}}
dip.data <- list ("J", "I", "lo.v", "hi.v", "DER", "IZQ")
dip.inits <- function (){
    list (
    v.star=vstar,
    x=rnorm(J),
    delta=rnorm(I),
    n=rnorm(I)
    )
    }
dip.parameters <- c("delta","n", "x")

#test ride to see program works
dip.60 <- bugs (dip.data, dip.inits, dip.parameters,
                "modelSta1Dj.txt", n.chains=3,
                n.iter=10, n.thin=1, debug=T,
                bugs.directory = "c:/Program Files (x86)/WinBUGS14/",
#                bugs.directory = "c:/Archivos de programa/WinBUGS14/",
                program = c("WinBUGS"))

#longer run
dip.60 <- bugs (dip.data, dip.inits, dip.parameters,
                "modelSta1Dj.txt", n.chains=3,
                n.iter=1000, n.thin=10, debug=T,
                bugs.directory = "c:/Program Files (x86)/WinBUGS14/",
#                bugs.directory = "c:/Archivos de programa/WinBUGS14/",
                program = c("WinBUGS"))

plot(dip.60)
print(dip.60)

#to continue running
tmp1<-list (
    v.star=vstar,
    x=dip.60$last.values[[1]]$x,
    delta=dip.60$last.values[[1]]$delta,
    n=dip.60$last.values[[1]]$n
    )
tmp2<-list (
    v.star=vstar,
    x=dip.60$last.values[[2]]$x,
    delta=dip.60$last.values[[2]]$delta,
    n=dip.60$last.values[[2]]$n
    )
tmp3<-list (
    v.star=vstar,
    x=dip.60$last.values[[3]]$x,
    delta=dip.60$last.values[[3]]$delta,
    n=dip.60$last.values[[3]]$n
    )
### for (chain in 1:3){dip.60$last.values[[chain]]$v.star <- vstar}
dip.60.2 <- bugs (dip.data,
                inits=list(tmp1,tmp2,tmp3),
                dip.parameters,
                "modelSta1Dj.txt", n.chains=3,
                n.iter=1000, n.thin=10, debug=T,
                bugs.directory = "c:/Program Files (x86)/WinBUGS14/",
#                bugs.directory = "c:/Archivos de programa/WinBUGS14/",
                program = c("WinBUGS"))

dip.60 <- dip.60.2; rm(dip.60.2)

attach.bugs(dip.60)
ip <- abs(x)

tmp <- rep(NA, times=J)
ips <- data.frame(p025=tmp, p50=tmp, p975=tmp, name=dip$name, part=dip$part, id=dip$id)
for (j in 1:J){
    ips[j,1] <- quantile (ip[,j], 0.025, names=F)
    ips[j,2] <- quantile (ip[,j], 0.50, names=F)
    ips[j,3] <- quantile (ip[,j], 0.975, names=F)
              }
ips <- ips[,c(-1,-3)]
ips[order(ips$p50),]


###############################################################
### all periods 66 members 2Dimensions DYNAMIC item anchors ###
###############################################################

votes <- RCs[,9:ncol(RCs)]
#votes[votes==-1] <- 0  # los -1s se vuelven 0s # DEJA ABSTENCION COMO VOTO NAY
votes <- t(votes)
J <- nrow(votes); I <- ncol(votes)
d1 <- ifelse(cuad==1,1,0) ##
d2 <- ifelse(cuad==2,1,0) ##
d3 <- ifelse(cuad==3,1,0) ##
d4 <- ifelse(cuad==4,1,0) ##
d5 <- ifelse(cuad==5,1,0) ##
d6 <- ifelse(cuad==6,1,0) ##
d7 <- ifelse(cuad==7,1,0) ##
d8 <- ifelse(cuad==8,1,0) ##
v <- votes
lo.v <- ifelse(is.na(v)==TRUE | v== 1, 0, -5)
hi.v <- ifelse(is.na(v)==TRUE | v==-1,  0, 5)
vstar <- matrix (NA, nrow=J, ncol=I)
for (j in 1:J){
for (i in 1:I){
  vstar[j,i] <- ifelse(v[j,i]==0, 0, ifelse(v[j,i]==1, runif(1), -1*runif(1)))}}
aldf.data <- list ("d1", "d2", "d3", "d4", "d5", "d6", "d7", "d8", "J", "I", "lo.v", "hi.v", "V1", "H1", "V2", "H2", "N")
##zero1<-rnorm(J); zero1[9]<-rnorm(1,-2); zero1[52]<-rnorm(1,2)  ## toma en cuenta priors j
##zero2<-rnorm(J); zero2[35]<-rnorm(1,2); zero2[64]<-rnorm(1,-2)
aldf.inits <- function (){
    list (
    v.star=vstar,
    alpha=rnorm(I),
    beta=rnorm(I),
    delta=rnorm(I),
#    xZero=zero1,
#    yZero=zero2,
    xOne=rnorm(J),
    yOne=rnorm(J),
    xTwo=rnorm(J),
    yTwo=rnorm(J),
    xThree=rnorm(J),
    yThree=rnorm(J),
    xFour=rnorm(J),
    yFour=rnorm(J),
    xFive=rnorm(J),
    yFive=rnorm(J),
    xSix=rnorm(J),
    ySix=rnorm(J),
    xSeven=rnorm(J),
    ySeven=rnorm(J),
    xEight=rnorm(J),
    yEight=rnorm(J)
    )
    }
aldf.parameters <- c("delta", "xOne", "xTwo", "xThree", "xFour", "xFive", "xSix", "xSeven", "xEight",
                              "yOne", "yTwo", "yThree", "yFour", "yFive", "ySix", "ySeven", "yEight",
                              "alpha", "beta", "delta")

#test ride to see program works
aldf.66 <- bugs (aldf.data, aldf.inits, aldf.parameters,
                "model66Dyn2Di4.irt.txt", n.chains=3,
                n.iter=20, n.thin=2, debug=T,
                bugs.directory = "c:/Archivos de programa/WinBUGS14/",
#                bugs.directory = "c:/Program Files (x86)/WinBUGS14/",
                program = c("WinBUGS"))

plot(aldf.66)
print(aldf.66)

#longer run
aldf.66 <- bugs (aldf.data, aldf.inits, aldf.parameters,
                "model66Dyn2Di4.irt.txt", n.chains=3,
                n.iter=10000, n.thin=20, debug=T,
                bugs.directory = "c:/Archivos de programa/WinBUGS14/",
#                bugs.directory = "c:/Program Files (x86)/WinBUGS14/",
                program = c("WinBUGS"))

#to continue running
tmp1<-list (
    v.star=vstar,
    alpha=aldf.66$last.values[[1]]$alpha,
    beta=aldf.66$last.values[[1]]$beta,
    delta=aldf.66$last.values[[1]]$delta,
#    xZero=aldf.66$last.values[[1]]$xZero,
#    yZero=aldf.66$last.values[[1]]$yZero,
    xOne=aldf.66$last.values[[1]]$xOne,
    yOne=aldf.66$last.values[[1]]$yOne,
    xTwo=aldf.66$last.values[[1]]$xTwo,
    yTwo=aldf.66$last.values[[1]]$yTwo,
    xThree=aldf.66$last.values[[1]]$xThree,
    yThree=aldf.66$last.values[[1]]$yThree,
    xFour=aldf.66$last.values[[1]]$xFour,
    yFour=aldf.66$last.values[[1]]$yFour,
    xFive=aldf.66$last.values[[1]]$xFive,
    yFive=aldf.66$last.values[[1]]$yFive,
    xSix=aldf.66$last.values[[1]]$xSix,
    ySix=aldf.66$last.values[[1]]$ySix,
    xSeven=aldf.66$last.values[[1]]$xSeven,
    ySeven=aldf.66$last.values[[1]]$ySeven,
    xEight=aldf.66$last.values[[1]]$xEight,
    yEight=aldf.66$last.values[[1]]$yEight
    )
tmp2<-list (
    v.star=vstar,
    alpha=aldf.66$last.values[[2]]$alpha,
    beta=aldf.66$last.values[[2]]$beta,
    delta=aldf.66$last.values[[2]]$delta,
#    xZero=aldf.66$last.values[[2]]$xZero,
#    yZero=aldf.66$last.values[[2]]$yZero,
    xOne=aldf.66$last.values[[2]]$xOne,
    yOne=aldf.66$last.values[[2]]$yOne,
    xTwo=aldf.66$last.values[[2]]$xTwo,
    yTwo=aldf.66$last.values[[2]]$yTwo,
    xThree=aldf.66$last.values[[2]]$xThree,
    yThree=aldf.66$last.values[[2]]$yThree,
    xFour=aldf.66$last.values[[2]]$xFour,
    yFour=aldf.66$last.values[[2]]$yFour,
    xFive=aldf.66$last.values[[2]]$xFive,
    yFive=aldf.66$last.values[[2]]$yFive,
    xSix=aldf.66$last.values[[2]]$xSix,
    ySix=aldf.66$last.values[[2]]$ySix,
    xSeven=aldf.66$last.values[[2]]$xSeven,
    ySeven=aldf.66$last.values[[2]]$ySeven,
    xEight=aldf.66$last.values[[2]]$xEight,
    yEight=aldf.66$last.values[[2]]$yEight
    )
tmp3<-list (
    v.star=vstar,
    alpha=aldf.66$last.values[[3]]$alpha,
    beta=aldf.66$last.values[[3]]$beta,
    delta=aldf.66$last.values[[3]]$delta,
#    xZero=aldf.66$last.values[[3]]$xZero,
#    yZero=aldf.66$last.values[[3]]$yZero,
    xOne=aldf.66$last.values[[3]]$xOne,
    yOne=aldf.66$last.values[[3]]$yOne,
    xTwo=aldf.66$last.values[[3]]$xTwo,
    yTwo=aldf.66$last.values[[3]]$yTwo,
    xThree=aldf.66$last.values[[3]]$xThree,
    yThree=aldf.66$last.values[[3]]$yThree,
    xFour=aldf.66$last.values[[3]]$xFour,
    yFour=aldf.66$last.values[[3]]$yFour,
    xFive=aldf.66$last.values[[3]]$xFive,
    yFive=aldf.66$last.values[[3]]$yFive,
    xSix=aldf.66$last.values[[3]]$xSix,
    ySix=aldf.66$last.values[[3]]$ySix,
    xSeven=aldf.66$last.values[[3]]$xSeven,
    ySeven=aldf.66$last.values[[3]]$ySeven,
    xEight=aldf.66$last.values[[3]]$xEight,
    yEight=aldf.66$last.values[[3]]$yEight
    )
### for (chain in 1:3){aldf.66$last.values[[chain]]$v.star <- vstar}
aldf.66.2 <- bugs (aldf.data,
                inits=list(tmp1,tmp2,tmp3),
                aldf.parameters,
                "model66Dyn2Di4.irt.txt", n.chains=3,
                n.iter=10000, n.thin=20, debug=T,
                bugs.directory = "c:/Program Files (x86)/WinBUGS14/",
                program = c("WinBUGS"))

aldf.66 <- aldf.66.2
rm(aldf.66.2)

plot(aldf.66)
print(aldf.66)

attach.bugs(aldf.66)
a <- -beta / delta ## pendiente cutline
b <- alpha / delta ## constante cutline
## 45-DEGREE CLOCKWISE ROTATION OF COORDINATES (SO THAT PRIORS REMAIN n, s, e, w)
xOneR <- xOne*cos(pi/4) + yOne*sin(pi/4)
yOneR <- -xOne*sin(pi/4) + yOne*cos(pi/4)
xTwoR <- xTwo*cos(pi/4) + yTwo*sin(pi/4)
yTwoR <- -xTwo*sin(pi/4) + yTwo*cos(pi/4)
xThreeR <- xThree*cos(pi/4) + yThree*sin(pi/4)
yThreeR <- -xThree*sin(pi/4) + yThree*cos(pi/4)
xFourR <- xFour*cos(pi/4) + yFour*sin(pi/4)
yFourR <- -xFour*sin(pi/4) + yFour*cos(pi/4)
xFiveR <- xFive*cos(pi/4) + yFive*sin(pi/4)
yFiveR <- -xFive*sin(pi/4) + yFive*cos(pi/4)
xSixR <- xSix*cos(pi/4) + ySix*sin(pi/4)
ySixR <- -xSix*sin(pi/4) + ySix*cos(pi/4)
xSevenR <- xSeven*cos(pi/4) + ySeven*sin(pi/4)
ySevenR <- -xSeven*sin(pi/4) + ySeven*cos(pi/4)
xEightR <- xEight*cos(pi/4) + yEight*sin(pi/4)
yEightR <- -xEight*sin(pi/4) + yEight*cos(pi/4)
xOne <- xOneR; yOne <- yOneR; xTwo <- xTwoR; yTwo <- yTwoR; xThree <- xThreeR; yThree <- yThreeR;
xFour <- xFourR; yFour <- yFourR; xFive <- xFiveR; yFive <- yFiveR; xSix <- xSixR; ySix <- ySixR;
xSeven <- xSevenR; ySeven <- ySevenR; xEight <- xEightR; yEight <- yEightR;
## 45-DEGREE CLOCKWISE ROTATION OF CUTLINES
xA <- -b/a; yA <- rep(0, length(a)); xO <- rep(0, length(a)); yO <- b  ## coords de Abscisa al origen y Ordenada al origan de c/cutline
xAR <- xA*cos(pi/4) + yA*sin(pi/4)
yAR <- -xA*sin(pi/4) + yA*cos(pi/4)
xOR <- xO*cos(pi/4) + yO*sin(pi/4)
yOR <- -xO*sin(pi/4) + yO*cos(pi/4)
X <- xAR; Y <- yAR; XX <- xOR; YY <- yOR ## simplifica notación
aR <- (YY-Y)/(XX-X)           ## pendiente del cutline rotado
bR <- YY -((YY-Y)/(XX-X))*XX  ## constante del cutline rotado
rm(X,Y,XX,YY)
a <- aR; b <- bR

jotas <- array(NA, dim=c(T,6,J))
for (j in 1:J){
    jotas[1,1,j] <- quantile (xOne[,j], 0.025, names=F)
    jotas[1,2,j] <- quantile (xOne[,j], 0.50, names=F)
    jotas[1,3,j] <- quantile (xOne[,j], 0.975, names=F)
    jotas[2,1,j] <- quantile (xTwo[,j], 0.025, names=F)
    jotas[2,2,j] <- quantile (xTwo[,j], 0.50, names=F)
    jotas[2,3,j] <- quantile (xTwo[,j], 0.975, names=F)
    jotas[3,1,j] <- quantile (xThree[,j], 0.025, names=F)
    jotas[3,2,j] <- quantile (xThree[,j], 0.50, names=F)
    jotas[3,3,j] <- quantile (xThree[,j], 0.975, names=F)
    jotas[4,1,j] <- quantile (xFour[,j], 0.025, names=F)
    jotas[4,2,j] <- quantile (xFour[,j], 0.50, names=F)
    jotas[4,3,j] <- quantile (xFour[,j], 0.975, names=F)
    jotas[5,1,j] <- quantile (xFive[,j], 0.025, names=F)
    jotas[5,2,j] <- quantile (xFive[,j], 0.50, names=F)
    jotas[5,3,j] <- quantile (xFive[,j], 0.975, names=F)
    jotas[6,1,j] <- quantile (xSix[,j], 0.025, names=F)
    jotas[6,2,j] <- quantile (xSix[,j], 0.50, names=F)
    jotas[6,3,j] <- quantile (xSix[,j], 0.975, names=F)
    jotas[7,1,j] <- quantile (xSeven[,j], 0.025, names=F)
    jotas[7,2,j] <- quantile (xSeven[,j], 0.50, names=F)
    jotas[7,3,j] <- quantile (xSeven[,j], 0.975, names=F)
    jotas[8,1,j] <- quantile (xEight[,j], 0.025, names=F)
    jotas[8,2,j] <- quantile (xEight[,j], 0.50, names=F)
    jotas[8,3,j] <- quantile (xEight[,j], 0.975, names=F)
    jotas[1,4,j] <- quantile (yOne[,j], 0.025, names=F)
    jotas[1,5,j] <- quantile (yOne[,j], 0.50, names=F)
    jotas[1,6,j] <- quantile (yOne[,j], 0.975, names=F)
    jotas[2,4,j] <- quantile (yTwo[,j], 0.025, names=F)
    jotas[2,5,j] <- quantile (yTwo[,j], 0.50, names=F)
    jotas[2,6,j] <- quantile (yTwo[,j], 0.975, names=F)
    jotas[3,4,j] <- quantile (yThree[,j], 0.025, names=F)
    jotas[3,5,j] <- quantile (yThree[,j], 0.50, names=F)
    jotas[3,6,j] <- quantile (yThree[,j], 0.975, names=F)
    jotas[4,4,j] <- quantile (yFour[,j], 0.025, names=F)
    jotas[4,5,j] <- quantile (yFour[,j], 0.50, names=F)
    jotas[4,6,j] <- quantile (yFour[,j], 0.975, names=F)
    jotas[5,4,j] <- quantile (yFive[,j], 0.025, names=F)
    jotas[5,5,j] <- quantile (yFive[,j], 0.50, names=F)
    jotas[5,6,j] <- quantile (yFive[,j], 0.975, names=F)
    jotas[6,4,j] <- quantile (ySix[,j], 0.025, names=F)
    jotas[6,5,j] <- quantile (ySix[,j], 0.50, names=F)
    jotas[6,6,j] <- quantile (ySix[,j], 0.975, names=F)
    jotas[7,4,j] <- quantile (ySeven[,j], 0.025, names=F)
    jotas[7,5,j] <- quantile (ySeven[,j], 0.50, names=F)
    jotas[7,6,j] <- quantile (ySeven[,j], 0.975, names=F)
    jotas[8,4,j] <- quantile (yEight[,j], 0.025, names=F)
    jotas[8,5,j] <- quantile (yEight[,j], 0.50, names=F)
    jotas[8,6,j] <- quantile (yEight[,j], 0.975, names=F)
    }
###Quita retiros y sustituciones
for (t in 8) jotas[t,,65] <- rep(NA,6) ## Jorge Díaz Cuervo pide licencia 23/9/2008=inicio cuad 7
for (t in 1:6) jotas[t,,66] <- rep(NA,6) ## Carla Sánchez Armas, la suplente

amed <- rep(NA,times=I)
bmed <- rep(NA,times=I)
for (i in 1:I){
    amed[i] <- quantile (a[,i], 0.50, names=F)
    bmed[i] <- quantile (b[,i], 0.50, names=F)  }

### Exporta coordenadas de todos los diputados
tmp <- matrix(NA, nrow=67, ncol=18)
tmp[,17] <- names.67
tmp[,18] <- part.67
for (j in 1:67){
    for (t in 1:8){
        tmp[j,t] <- jotas[t,2,j]
        tmp[j,t+8] <- jotas[t,5,j]
        }}
tmp<-data.matrix(tmp)
for (n in 1:16){tmp[,n] <- as.numeric(tmp[,n])}
write.table(tmp, file="aldfIdPts.xls", sep=",")
### Exporta
tmp <- data.frame(cbind(yr=RCs$yr, mo=RCs$mo, dy=RCs$dy ,a=amed, b=bmed))
write.table(tmp, file="aldfCutlines.xls", sep=",")

### TO RESET GRAPH PARAMETERS SAY par(oldpar) ###
oldpar <- par(no.readonly=TRUE)

#par(mfrow=c(3,3))
#par("pin" = c(.63,.58)) #width and height of plot region in inches

## FUNCTION TO DRAW ELLIPSES OVOIDS
ellipsePoints <- function(a,b, alpha = 0, loc = c(0,0), n = 201)
{
    ## Purpose: ellipse points,radially equispaced, given geometric par.s
    ## -------------------------------------------------------------------------
    ## Arguments: a, b : length of half axes in (x,y) direction
    ##            alpha: angle (in degrees) for rotation
    ##            loc  : center of ellipse
    ##            n    : number of points
    ## -------------------------------------------------------------------------
    ## Author: Martin Maechler, Date: 19 Mar 2002, 16:26
    B <- min(a,b)
    A <- max(a,b)
    ## B <= A
    d2 <- (A-B)*(A+B)                   #= A^2 - B^2
    phi <- 2*pi*seq(0,1, len = n)
    sp <- sin(phi)
    cp <- cos(phi)
    r <- a*b / sqrt(B^2 + d2 * sp^2)
    xy <- r * cbind(cp, sp)
    ## xy are the ellipse points for alpha = 0 and loc = (0,0)
    al <- alpha * pi/180
    ca <- cos(al)
    sa <- sin(al)
    xy %*% rbind(c(ca, sa), c(-sa, ca)) + cbind(rep(loc[1],n),
                                                rep(loc[2],n))
}


tmp <- c(jotas[,1,1],jotas[,4,1],jotas[,1,2],jotas[,4,2],jotas[,1,3],jotas[,4,3],jotas[,1,4],jotas[,4,4],
        jotas[,1,5],jotas[,4,5],jotas[,1,6],jotas[,4,6],jotas[,1,7],jotas[,4,7],jotas[,1,8],jotas[,4,8],
        jotas[,1,9],jotas[,4,9],jotas[,1,10],jotas[,4,10],jotas[,1,11],jotas[,4,11],jotas[,1,12],jotas[,4,12],
        jotas[,1,13],jotas[,4,13],jotas[,1,14],jotas[,4,14],jotas[,1,15],jotas[,4,15],jotas[,1,16],jotas[,4,16],
        jotas[,1,17],jotas[,4,17],jotas[,1,18],jotas[,4,18],jotas[,1,19],jotas[,4,19],jotas[,1,20],jotas[,4,20],
        jotas[,1,21],jotas[,4,21],jotas[,1,22],jotas[,4,22],jotas[,1,23],jotas[,4,23],jotas[,1,24],jotas[,4,24],
        jotas[,1,25],jotas[,4,25],jotas[,1,26],jotas[,4,26],jotas[,1,27],jotas[,4,27],jotas[,1,28],jotas[,4,28],
        jotas[,1,29],jotas[,4,29],jotas[,1,30],jotas[,4,30],jotas[,1,31],jotas[,4,31],jotas[,1,32],jotas[,4,32],
        jotas[,1,33],jotas[,4,33],jotas[,1,34],jotas[,4,34],jotas[,1,35],jotas[,4,35],jotas[,1,36],jotas[,4,36],
        jotas[,1,37],jotas[,4,37],jotas[,1,38],jotas[,4,38],jotas[,1,39],jotas[,4,39],jotas[,1,40],jotas[,4,40],
        jotas[,1,41],jotas[,4,41],jotas[,1,42],jotas[,4,42],jotas[,1,43],jotas[,4,43],jotas[,1,44],jotas[,4,44],
        jotas[,1,45],jotas[,4,45],jotas[,1,46],jotas[,4,46],jotas[,1,47],jotas[,4,47],jotas[,1,48],jotas[,4,48],
        jotas[,1,49],jotas[,4,49],jotas[,1,50],jotas[,4,50],jotas[,1,51],jotas[,4,51],jotas[,1,52],jotas[,4,52],
        jotas[,1,53],jotas[,4,53],jotas[,1,54],jotas[,4,54],jotas[,1,55],jotas[,4,55],jotas[,1,56],jotas[,4,56],
        jotas[,1,57],jotas[,4,57],jotas[,1,58],jotas[,4,58],jotas[,1,59],jotas[,4,59],jotas[,1,60],jotas[,4,60],
        jotas[,1,61],jotas[,4,61],jotas[,1,62],jotas[,4,62],jotas[,1,63],jotas[,4,63],jotas[,1,64],jotas[,4,64],
        jotas[,1,65],jotas[,4,65],jotas[,1,66],jotas[,4,66],jotas[,1,67],jotas[,4,67])
for (i in 1:length(tmp)) { tmp[i] <- ifelse(is.na(tmp[i])==TRUE,0,tmp[i]) }
min <- min( tmp )
tmp <- c(jotas[,3,1],jotas[,6,1],jotas[,3,2],jotas[,6,2],jotas[,3,3],jotas[,6,3],jotas[,3,4],jotas[,6,4],
        jotas[,3,5],jotas[,6,5],jotas[,3,6],jotas[,6,6],jotas[,3,7],jotas[,6,7],jotas[,3,8],jotas[,6,8],
        jotas[,3,9],jotas[,6,9],jotas[,3,10],jotas[,6,10],jotas[,3,11],jotas[,6,11],jotas[,3,12],jotas[,6,12],
        jotas[,3,13],jotas[,6,13],jotas[,3,14],jotas[,6,14],jotas[,3,15],jotas[,6,15],jotas[,3,16],jotas[,6,16],
        jotas[,3,17],jotas[,6,17],jotas[,3,18],jotas[,6,18],jotas[,3,19],jotas[,6,19],jotas[,3,20],jotas[,6,20],
        jotas[,3,21],jotas[,6,21],jotas[,3,22],jotas[,6,22],jotas[,3,23],jotas[,6,23],jotas[,3,24],jotas[,6,24],
        jotas[,3,25],jotas[,6,25],jotas[,3,26],jotas[,6,26],jotas[,3,27],jotas[,6,27],jotas[,3,28],jotas[,6,28],
        jotas[,3,29],jotas[,6,29],jotas[,3,30],jotas[,6,30],jotas[,3,31],jotas[,6,31],jotas[,3,32],jotas[,6,32],
        jotas[,3,33],jotas[,6,33],jotas[,3,34],jotas[,6,34],jotas[,3,35],jotas[,6,35],jotas[,3,36],jotas[,6,36],
        jotas[,3,37],jotas[,6,37],jotas[,3,38],jotas[,6,38],jotas[,3,39],jotas[,6,39],jotas[,3,40],jotas[,6,40],
        jotas[,3,41],jotas[,6,41],jotas[,3,42],jotas[,6,42],jotas[,3,43],jotas[,6,43],jotas[,3,44],jotas[,6,44],
        jotas[,3,45],jotas[,6,45],jotas[,3,46],jotas[,6,46],jotas[,3,47],jotas[,6,47],jotas[,3,48],jotas[,6,48],
        jotas[,3,49],jotas[,6,49],jotas[,3,50],jotas[,6,50],jotas[,3,51],jotas[,6,51],jotas[,3,52],jotas[,6,52],
        jotas[,3,53],jotas[,6,53],jotas[,3,54],jotas[,6,54],jotas[,3,55],jotas[,6,55],jotas[,3,56],jotas[,6,56],
        jotas[,3,57],jotas[,6,57],jotas[,3,58],jotas[,6,58],jotas[,3,59],jotas[,6,59],jotas[,3,60],jotas[,6,60],
        jotas[,3,61],jotas[,6,61],jotas[,3,62],jotas[,6,62],jotas[,3,63],jotas[,6,63],jotas[,3,64],jotas[,6,64],
        jotas[,3,65],jotas[,6,65],jotas[,3,66],jotas[,6,66],jotas[,3,67],jotas[,6,67])
for (i in 1:length(tmp)) { tmp[i] <- ifelse(is.na(tmp[i])==TRUE,0,tmp[i]) }
max <- max( tmp )
lims <- c(NA,NA)
lims[1] <- ifelse(abs(min)>max, min, -max)
lims[2] <- ifelse(abs(min)>max, abs(min), max)

#### FOR USE IF ELLIPSES WILL BE GRAPHED
##eps <- array(NA, dim=c(201,2,67))
##eps[,,1] <- ellipsePoints(a=jotas[t,2,1]-jotas[t,1,1],b=jotas[t,5,1]-jotas[t,4,1],alpha=0,loc=c(jotas[t,2,1],jotas[t,5,1]),n=201)
##eps[,,2] <- ellipsePoints(a=jotas[t,2,2]-jotas[t,1,2],b=jotas[t,5,2]-jotas[t,4,2],alpha=0,loc=c(jotas[t,2,2],jotas[t,5,2]),n=201)
##eps[,,3] <- ellipsePoints(a=jotas[t,2,3]-jotas[t,1,3],b=jotas[t,5,3]-jotas[t,4,3],alpha=0,loc=c(jotas[t,2,3],jotas[t,5,3]),n=201)
##eps[,,4] <- ellipsePoints(a=jotas[t,2,4]-jotas[t,1,4],b=jotas[t,5,4]-jotas[t,4,4],alpha=0,loc=c(jotas[t,2,4],jotas[t,5,4]),n=201)
##eps[,,5] <- ellipsePoints(a=jotas[t,2,5]-jotas[t,1,5],b=jotas[t,5,5]-jotas[t,4,5],alpha=0,loc=c(jotas[t,2,5],jotas[t,5,5]),n=201)
##eps[,,6] <- ellipsePoints(a=jotas[t,2,6]-jotas[t,1,6],b=jotas[t,5,6]-jotas[t,4,6],alpha=0,loc=c(jotas[t,2,6],jotas[t,5,6]),n=201)
##eps[,,7] <- ellipsePoints(a=jotas[t,2,7]-jotas[t,1,7],b=jotas[t,5,7]-jotas[t,4,7],alpha=0,loc=c(jotas[t,2,7],jotas[t,5,7]),n=201)
##eps[,,8] <- ellipsePoints(a=jotas[t,2,8]-jotas[t,1,8],b=jotas[t,5,8]-jotas[t,4,8],alpha=0,loc=c(jotas[t,2,8],jotas[t,5,8]),n=201)
##eps[,,9] <- ellipsePoints(a=jotas[t,2,9]-jotas[t,1,9],b=jotas[t,5,9]-jotas[t,4,9],alpha=0,loc=c(jotas[t,2,9],jotas[t,5,9]),n=201)
##eps[,,10] <- ellipsePoints(a=jotas[t,2,10]-jotas[t,1,10],b=jotas[t,5,10]-jotas[t,4,10],alpha=0,loc=c(jotas[t,2,10],jotas[t,5,10]),n=201)
##eps[,,11] <- ellipsePoints(a=jotas[t,2,11]-jotas[t,1,11],b=jotas[t,5,11]-jotas[t,4,11],alpha=0,loc=c(jotas[t,2,11],jotas[t,5,11]),n=201)
##eps[,,12] <- ellipsePoints(a=jotas[t,2,12]-jotas[t,1,12],b=jotas[t,5,12]-jotas[t,4,12],alpha=0,loc=c(jotas[t,2,12],jotas[t,5,12]),n=201)
##eps[,,13] <- ellipsePoints(a=jotas[t,2,13]-jotas[t,1,13],b=jotas[t,5,13]-jotas[t,4,13],alpha=0,loc=c(jotas[t,2,13],jotas[t,5,13]),n=201)
##eps[,,14] <- ellipsePoints(a=jotas[t,2,14]-jotas[t,1,14],b=jotas[t,5,14]-jotas[t,4,14],alpha=0,loc=c(jotas[t,2,14],jotas[t,5,14]),n=201)
##eps[,,15] <- ellipsePoints(a=jotas[t,2,15]-jotas[t,1,15],b=jotas[t,5,15]-jotas[t,4,15],alpha=0,loc=c(jotas[t,2,15],jotas[t,5,15]),n=201)
##eps[,,16] <- ellipsePoints(a=jotas[t,2,16]-jotas[t,1,16],b=jotas[t,5,16]-jotas[t,4,16],alpha=0,loc=c(jotas[t,2,16],jotas[t,5,16]),n=201)
##eps[,,17] <- ellipsePoints(a=jotas[t,2,17]-jotas[t,1,17],b=jotas[t,5,17]-jotas[t,4,17],alpha=0,loc=c(jotas[t,2,17],jotas[t,5,17]),n=201)
##eps[,,18] <- ellipsePoints(a=jotas[t,2,18]-jotas[t,1,18],b=jotas[t,5,18]-jotas[t,4,18],alpha=0,loc=c(jotas[t,2,18],jotas[t,5,18]),n=201)
##eps[,,19] <- ellipsePoints(a=jotas[t,2,19]-jotas[t,1,19],b=jotas[t,5,19]-jotas[t,4,19],alpha=0,loc=c(jotas[t,2,19],jotas[t,5,19]),n=201)
##eps[,,20] <- ellipsePoints(a=jotas[t,2,20]-jotas[t,1,20],b=jotas[t,5,20]-jotas[t,4,20],alpha=0,loc=c(jotas[t,2,20],jotas[t,5,20]),n=201)
##eps[,,21] <- ellipsePoints(a=jotas[t,2,21]-jotas[t,1,21],b=jotas[t,5,21]-jotas[t,4,21],alpha=0,loc=c(jotas[t,2,21],jotas[t,5,21]),n=201)
##eps[,,22] <- ellipsePoints(a=jotas[t,2,22]-jotas[t,1,22],b=jotas[t,5,22]-jotas[t,4,22],alpha=0,loc=c(jotas[t,2,22],jotas[t,5,22]),n=201)
##eps[,,23] <- ellipsePoints(a=jotas[t,2,23]-jotas[t,1,23],b=jotas[t,5,23]-jotas[t,4,23],alpha=0,loc=c(jotas[t,2,23],jotas[t,5,23]),n=201)
##eps[,,24] <- ellipsePoints(a=jotas[t,2,24]-jotas[t,1,24],b=jotas[t,5,24]-jotas[t,4,24],alpha=0,loc=c(jotas[t,2,24],jotas[t,5,24]),n=201)
##eps[,,25] <- ellipsePoints(a=jotas[t,2,25]-jotas[t,1,25],b=jotas[t,5,25]-jotas[t,4,25],alpha=0,loc=c(jotas[t,2,25],jotas[t,5,25]),n=201)
##eps[,,26] <- ellipsePoints(a=jotas[t,2,26]-jotas[t,1,26],b=jotas[t,5,26]-jotas[t,4,26],alpha=0,loc=c(jotas[t,2,26],jotas[t,5,26]),n=201)
##eps[,,27] <- ellipsePoints(a=jotas[t,2,27]-jotas[t,1,27],b=jotas[t,5,27]-jotas[t,4,27],alpha=0,loc=c(jotas[t,2,27],jotas[t,5,27]),n=201)
##eps[,,28] <- ellipsePoints(a=jotas[t,2,28]-jotas[t,1,28],b=jotas[t,5,28]-jotas[t,4,28],alpha=0,loc=c(jotas[t,2,28],jotas[t,5,28]),n=201)
##eps[,,29] <- ellipsePoints(a=jotas[t,2,29]-jotas[t,1,29],b=jotas[t,5,29]-jotas[t,4,29],alpha=0,loc=c(jotas[t,2,29],jotas[t,5,29]),n=201)
##eps[,,30] <- ellipsePoints(a=jotas[t,2,30]-jotas[t,1,30],b=jotas[t,5,30]-jotas[t,4,30],alpha=0,loc=c(jotas[t,2,30],jotas[t,5,30]),n=201)
##eps[,,31] <- ellipsePoints(a=jotas[t,2,31]-jotas[t,1,31],b=jotas[t,5,31]-jotas[t,4,31],alpha=0,loc=c(jotas[t,2,31],jotas[t,5,31]),n=201)
##eps[,,32] <- ellipsePoints(a=jotas[t,2,32]-jotas[t,1,32],b=jotas[t,5,32]-jotas[t,4,32],alpha=0,loc=c(jotas[t,2,32],jotas[t,5,32]),n=201)
##eps[,,33] <- ellipsePoints(a=jotas[t,2,33]-jotas[t,1,33],b=jotas[t,5,33]-jotas[t,4,33],alpha=0,loc=c(jotas[t,2,33],jotas[t,5,33]),n=201)
##eps[,,34] <- ellipsePoints(a=jotas[t,2,34]-jotas[t,1,34],b=jotas[t,5,34]-jotas[t,4,34],alpha=0,loc=c(jotas[t,2,34],jotas[t,5,34]),n=201)
##eps[,,35] <- ellipsePoints(a=jotas[t,2,35]-jotas[t,1,35],b=jotas[t,5,35]-jotas[t,4,35],alpha=0,loc=c(jotas[t,2,35],jotas[t,5,35]),n=201)
##eps[,,36] <- ellipsePoints(a=jotas[t,2,36]-jotas[t,1,36],b=jotas[t,5,36]-jotas[t,4,36],alpha=0,loc=c(jotas[t,2,36],jotas[t,5,36]),n=201)
##eps[,,37] <- ellipsePoints(a=jotas[t,2,37]-jotas[t,1,37],b=jotas[t,5,37]-jotas[t,4,37],alpha=0,loc=c(jotas[t,2,37],jotas[t,5,37]),n=201)
##eps[,,38] <- ellipsePoints(a=jotas[t,2,38]-jotas[t,1,38],b=jotas[t,5,38]-jotas[t,4,38],alpha=0,loc=c(jotas[t,2,38],jotas[t,5,38]),n=201)
##eps[,,39] <- ellipsePoints(a=jotas[t,2,39]-jotas[t,1,39],b=jotas[t,5,39]-jotas[t,4,39],alpha=0,loc=c(jotas[t,2,39],jotas[t,5,39]),n=201)
##eps[,,40] <- ellipsePoints(a=jotas[t,2,40]-jotas[t,1,40],b=jotas[t,5,40]-jotas[t,4,40],alpha=0,loc=c(jotas[t,2,40],jotas[t,5,40]),n=201)
##eps[,,41] <- ellipsePoints(a=jotas[t,2,41]-jotas[t,1,41],b=jotas[t,5,41]-jotas[t,4,41],alpha=0,loc=c(jotas[t,2,41],jotas[t,5,41]),n=201)
##eps[,,42] <- ellipsePoints(a=jotas[t,2,42]-jotas[t,1,42],b=jotas[t,5,42]-jotas[t,4,42],alpha=0,loc=c(jotas[t,2,42],jotas[t,5,42]),n=201)
##eps[,,43] <- ellipsePoints(a=jotas[t,2,43]-jotas[t,1,43],b=jotas[t,5,43]-jotas[t,4,43],alpha=0,loc=c(jotas[t,2,43],jotas[t,5,43]),n=201)
##eps[,,44] <- ellipsePoints(a=jotas[t,2,44]-jotas[t,1,44],b=jotas[t,5,44]-jotas[t,4,44],alpha=0,loc=c(jotas[t,2,44],jotas[t,5,44]),n=201)
##eps[,,45] <- ellipsePoints(a=jotas[t,2,45]-jotas[t,1,45],b=jotas[t,5,45]-jotas[t,4,45],alpha=0,loc=c(jotas[t,2,45],jotas[t,5,45]),n=201)
##eps[,,46] <- ellipsePoints(a=jotas[t,2,46]-jotas[t,1,46],b=jotas[t,5,46]-jotas[t,4,46],alpha=0,loc=c(jotas[t,2,46],jotas[t,5,46]),n=201)
##eps[,,47] <- ellipsePoints(a=jotas[t,2,47]-jotas[t,1,47],b=jotas[t,5,47]-jotas[t,4,47],alpha=0,loc=c(jotas[t,2,47],jotas[t,5,47]),n=201)
##eps[,,48] <- ellipsePoints(a=jotas[t,2,48]-jotas[t,1,48],b=jotas[t,5,48]-jotas[t,4,48],alpha=0,loc=c(jotas[t,2,48],jotas[t,5,48]),n=201)
##eps[,,49] <- ellipsePoints(a=jotas[t,2,49]-jotas[t,1,49],b=jotas[t,5,49]-jotas[t,4,49],alpha=0,loc=c(jotas[t,2,49],jotas[t,5,49]),n=201)
##eps[,,50] <- ellipsePoints(a=jotas[t,2,50]-jotas[t,1,50],b=jotas[t,5,50]-jotas[t,4,50],alpha=0,loc=c(jotas[t,2,50],jotas[t,5,50]),n=201)
##eps[,,51] <- ellipsePoints(a=jotas[t,2,51]-jotas[t,1,51],b=jotas[t,5,51]-jotas[t,4,51],alpha=0,loc=c(jotas[t,2,51],jotas[t,5,51]),n=201)
##eps[,,52] <- ellipsePoints(a=jotas[t,2,52]-jotas[t,1,52],b=jotas[t,5,52]-jotas[t,4,52],alpha=0,loc=c(jotas[t,2,52],jotas[t,5,52]),n=201)
##eps[,,53] <- ellipsePoints(a=jotas[t,2,53]-jotas[t,1,53],b=jotas[t,5,53]-jotas[t,4,53],alpha=0,loc=c(jotas[t,2,53],jotas[t,5,53]),n=201)
##eps[,,54] <- ellipsePoints(a=jotas[t,2,54]-jotas[t,1,54],b=jotas[t,5,54]-jotas[t,4,54],alpha=0,loc=c(jotas[t,2,54],jotas[t,5,54]),n=201)
##eps[,,55] <- ellipsePoints(a=jotas[t,2,55]-jotas[t,1,55],b=jotas[t,5,55]-jotas[t,4,55],alpha=0,loc=c(jotas[t,2,55],jotas[t,5,55]),n=201)
##eps[,,56] <- ellipsePoints(a=jotas[t,2,56]-jotas[t,1,56],b=jotas[t,5,56]-jotas[t,4,56],alpha=0,loc=c(jotas[t,2,56],jotas[t,5,56]),n=201)
##eps[,,57] <- ellipsePoints(a=jotas[t,2,57]-jotas[t,1,57],b=jotas[t,5,57]-jotas[t,4,57],alpha=0,loc=c(jotas[t,2,57],jotas[t,5,57]),n=201)
##eps[,,58] <- ellipsePoints(a=jotas[t,2,58]-jotas[t,1,58],b=jotas[t,5,58]-jotas[t,4,58],alpha=0,loc=c(jotas[t,2,58],jotas[t,5,58]),n=201)
##eps[,,59] <- ellipsePoints(a=jotas[t,2,59]-jotas[t,1,59],b=jotas[t,5,59]-jotas[t,4,59],alpha=0,loc=c(jotas[t,2,59],jotas[t,5,59]),n=201)
##eps[,,60] <- ellipsePoints(a=jotas[t,2,60]-jotas[t,1,60],b=jotas[t,5,60]-jotas[t,4,60],alpha=0,loc=c(jotas[t,2,60],jotas[t,5,60]),n=201)
##eps[,,61] <- ellipsePoints(a=jotas[t,2,61]-jotas[t,1,61],b=jotas[t,5,61]-jotas[t,4,61],alpha=0,loc=c(jotas[t,2,61],jotas[t,5,61]),n=201)
##eps[,,62] <- ellipsePoints(a=jotas[t,2,62]-jotas[t,1,62],b=jotas[t,5,62]-jotas[t,4,62],alpha=0,loc=c(jotas[t,2,62],jotas[t,5,62]),n=201)
##eps[,,63] <- ellipsePoints(a=jotas[t,2,63]-jotas[t,1,63],b=jotas[t,5,63]-jotas[t,4,63],alpha=0,loc=c(jotas[t,2,63],jotas[t,5,63]),n=201)
##eps[,,64] <- ellipsePoints(a=jotas[t,2,64]-jotas[t,1,64],b=jotas[t,5,64]-jotas[t,4,64],alpha=0,loc=c(jotas[t,2,64],jotas[t,5,64]),n=201)
##eps[,,65] <- ellipsePoints(a=jotas[t,2,65]-jotas[t,1,65],b=jotas[t,5,65]-jotas[t,4,65],alpha=0,loc=c(jotas[t,2,65],jotas[t,5,65]),n=201)
##eps[,,66] <- ellipsePoints(a=jotas[t,2,66]-jotas[t,1,66],b=jotas[t,5,66]-jotas[t,4,66],alpha=0,loc=c(jotas[t,2,66],jotas[t,5,66]),n=201)
##eps[,,67] <- ellipsePoints(a=jotas[t,2,67]-jotas[t,1,67],b=jotas[t,5,67]-jotas[t,4,67],alpha=0,loc=c(jotas[t,2,67],jotas[t,5,67]),n=201)

### SET t
t <- 5
for (t in 1:T){
par(mar = c(3.1, 3.1, 2.1, 2.1) )
##plot(c(-1.5,1.5),c(-1.5,1.5),xlim = lims,ylim = lims,type="n",
##       xlab=c(""), ##xlab=c("pro-SQ                                         pro-change"),
##       ylab=c(""), ##ylab=c("interpretivist                                            literalist"),
##       main=c("")) ##main=paste("Acc+Con (ancla j) time=",t,I,"obs"))
plot(c(-1.5,1.5),c(-1.5,1.5),type="n",
       xlab=c(""), ##xlab=c("pro-SQ                                         pro-change"),
       ylab=c(""), ##ylab=c("interpretivist                                            literalist"),
       main=c("")) ##main=paste("Acc+Con (ancla j) time=",t,I,"obs"))
abline(-1.5,0,col="grey",lty=3); abline(-1,0,col="grey",lty=3); abline(-.5,0,col="grey",lty=3);
       abline(0,0,col="grey",lty=3); abline(.5,0,col="grey",lty=3); abline(1,0,col="grey",lty=3);
       abline(1.5,0,col="grey",lty=3);
abline(v=-1.5,col="grey",lty=3); abline(v=-1,col="grey",lty=3); abline(v=-.5,col="grey",lty=3);
       abline(v=0,col="grey",lty=3); abline(v=.5,col="grey",lty=3); abline(v=1,col="grey",lty=3);
       abline(v=1.5,col="grey",lty=3);
legend(1.1,-.65, legend=part.list, cex=.75, pch=20, pt.cex=1.25, col=color.list, bg="white")
##for (j in 1:J){
##    segments(jotas[t,1,j],jotas[t,5,j],jotas[t,3,j],jotas[t,5,j],col="gray")
##    segments(jotas[t,2,j],jotas[t,4,j],jotas[t,2,j],jotas[t,6,j],col="gray")
##    }
##for (j in 1:J){
##    lines(eps[,,j],col=color.67[j])
##    }
for (j in 1:J){
    points(jotas[t,2,j],jotas[t,5,j],pch=20,col=color.67[j])
    }
for (j in 1:J){
    points(jotas[t,2,j],jotas[t,5,j], col=dCoord[j]); ## pone coordinadores
    }
##for (j in 1:J){
##    text(jotas[t,2,j],jotas[t,5,j],labels=coords[j])
##    }
tmp <- c("2006-3","2007-1","2007-2","2007-3","2008-1","2008-2","2008-3","2009-1")
setwd("d:/01/data/rollcall/aldf/graphs")
savePlot(filename = paste(tmp[t], sep=""), type = "pdf")
#setwd("c:/data")
setwd("d:/01/data/rollcall/aldf")
}

## cutlines
cuad <- 1*d1+2*d2+3*d3+4*d4+5*d5+6*d6+7*d7+8*d8
t <- 5
for (t in 1:8){
    plot(c(-1.5,1.5),c(-1.5,1.5),type="n",
           xlab=c(""), ##xlab=c("pro-SQ                                         pro-change"),
           ylab=c(""), ##ylab=c("interpretivist                                            literalist"),
           main=c("")) ##main=paste("Acc+Con (ancla j) time=",t,I,"obs"))
    atmp <- amed[cuad==t]; btmp <- bmed[cuad==t];
    N <- length(atmp)
    for (n in 1:N){
        abline(a=btmp[n],b=atmp[n]) } ## OJO: a en mi modelo es slope, en R es constant
    for (j in 1:J){
        points(jotas[t,2,j],jotas[t,5,j],pch=20,col=color.67[j])
        }
    tmp <- c("2006-3","2007-1","2007-2","2007-3","2008-1","2008-2","2008-3","2009-1")
    setwd("d:/01/data/rollcall/aldf/graphs")
    savePlot(filename = paste(tmp[t], "cutlines", sep=""), type = "pdf")
    setwd("d:/01/data/rollcall/aldf")
              }





# Prob of being median
attach.bugs(trife.edos.1)
is.median <- matrix(NA, nrow=3000, ncol=7)
med <- rep(NA, times=3000)
for (i in 1:3000){
    med[i]<-median(x[i,1:7])}
for (i in 1:3000){
    for (j in 1:7){
        is.median[i,j] <- ifelse(x[i,j]==med[i],1,0)}}
pr.median <- rep(NA, times=7, names=names.1)
for (j in 1:7){
    pr.median[j] <- sum(is.median[,j]/3000)}
names.1
pr.median


#HISTOGRAM OF POSTERIOR m
oldpar <- par(no.readonly=TRUE)

attach.bugs(trife.all.1)

#par(mfrow=c(2,1))
#par(oldpar)

eme <- c(m[,1],m[,2],m[,3],m[,4],m[,5],m[,6],m[,7])
#equis <- c(-400:400)/100
hist(eme[abs(eme)<=3], col="gray", xlim=c(-3,3), ylim=c(-.04,.55),freq=FALSE, main="m_i's prior and posterior densities", xlab=NULL)
points(all.1.80[,2],rep(0,times=7), pch=20)
curve(dnorm, from=-3, to=3, add=TRUE)
text(all.1.80[1,2],-.03,label="Reyes")
text(all.1.80[7,2],-.03,label="Fuentes")
#densityplot(eme[abs(eme)<=4])

#######################################################
### Static 66 members 2Dimensions extremist anchors ###
#######################################################

### ALLOWS TO DROP CASES FROM ANALYSIS
#year1 <- ifelse( (cuad==1 | cuad==2 | cuad==3), 1, 0 ) ## FALTA QUITAR A UN DIPUTADO (MÁS?) QUE NO ENTRÓ HASTA DESPUÉS
#drop <- ifelse(year1==0,1,0)
#tmp<-RCs[drop==0,]
#RCs<-tmp
#sem<-sem[drop==0]; cuad<-cuad[drop==0]; trim<-trim[drop==0]

votes <- RCs[,9:ncol(RCs)]
#votes[votes==-1] <- 0  # los -1s se vuelven 0s # DEJA ABSTENCION COMO VOTO NAY
votes <- t(votes)
J <- nrow(votes); I <- ncol(votes)
v <- votes
lo.v <- ifelse(is.na(v)==TRUE | v== 1, 0, -5)
hi.v <- ifelse(is.na(v)==TRUE | v==-1,  0, 5)
vstar <- matrix (NA, nrow=J, ncol=I)
for (j in 1:J){
for (i in 1:I){
  vstar[j,i] <- ifelse(v[j,i]==0, 0, ifelse(v[j,i]==1, runif(1), -1*runif(1)))}}
aldf.data <- list ("J", "I", "lo.v", "hi.v", "N", "W", "E", "S")
aldf.inits <- function (){
    list (
    v.star=vstar,
    delta=rnorm(I),
    angle=runif(I),
    b=rnorm(I)
#    x=rnorm(J),
#    y=rnorm(J)
    )
    }
aldf.parameters <- c("delta", "x", "y", "a", "b", "angle")

#test ride to see program works
aldf.66 <- bugs (aldf.data, aldf.inits, aldf.parameters,
                "model66Sta2Dj.txt", n.chains=3,
                n.iter=20, n.thin=2, debug=T,
                bugs.directory = "c:/Program Files (x86)/WinBUGS14/",
                program = c("WinBUGS"))

plot(aldf.66)
print(aldf.66)

#longer run
aldf.66 <- bugs (aldf.data, aldf.inits, aldf.parameters,
                "model66Sta2Dj.txt", n.chains=3,
                n.iter=10000, n.thin=25, debug=T,
                bugs.directory = "c:/Program Files (x86)/WinBUGS14/",
                program = c("WinBUGS"))

#to continue running
tmp1<-list (
    v.star=vstar,
    delta=aldf.66$last.values[[1]]$delta,
    angle=aldf.66$last.values[[1]]$angle,
    b=aldf.66$last.values[[1]]$b,
    x=aldf.66$last.values[[1]]$x,
    y=aldf.66$last.values[[1]]$y
    )
tmp2<-list (
    v.star=vstar,
    delta=aldf.66$last.values[[2]]$delta,
    angle=aldf.66$last.values[[2]]$angle,
    b=aldf.66$last.values[[2]]$b,
    x=aldf.66$last.values[[2]]$x,
    y=aldf.66$last.values[[2]]$y
    )
tmp3<-list (
    v.star=vstar,
    delta=aldf.66$last.values[[3]]$delta,
    angle=aldf.66$last.values[[3]]$angle,
    b=aldf.66$last.values[[3]]$b,
    x=aldf.66$last.values[[3]]$x,
    y=aldf.66$last.values[[3]]$y
    )
### for (chain in 1:3){aldf.66$last.values[[chain]]$v.star <- vstar}
aldf.66.2 <- bugs (aldf.data,
                inits=list(tmp1,tmp2,tmp3),
                aldf.parameters,
                "model66Sta2Dj.txt", n.chains=3,
                n.iter=25000, n.thin=60, debug=T,
                bugs.directory = "c:/Program Files (x86)/WinBUGS14/",
                program = c("WinBUGS"))

aldf.66 <- aldf.66.2
rm(aldf.66.2)

