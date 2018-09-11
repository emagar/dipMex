# Invoca Jags desde R
##
library (R2jags)
#library (runjags)
library (foreign)
library (car)
library (gtools)
library (MCMCpack)
#library (snowfall)
#library (rlecuyer)
#library(arm)

rm(list = ls())
##
#workdir <- c("/media/shared/01/Dropbox/data/rollcall/dipMex")
workdir <- c("d:/01/Dropbox/data/rollcall/dipMex")
#workdir <- c("C:/Documents and Settings/emagarm/Mis documentos/My Dropbox/data/rollcall/dipMex")
#workdir <- c("C:/Documents and Settings/emm/Mis documentos/My Dropbox/data/rollcall/dipMex")
setwd(workdir)
##
#set.seed(1970)

load(file = paste(workdir, "votesForWeb/rc60.RData", sep = "/"))
dipdat$id <- as.character(dipdat$id)
dipdat$part <- as.character(dipdat$part)
dipdat$part[dipdat$part=="asd"] <- "psd"
##
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

### WORK WITH SAMPLE OF VOTES IF SO WISHED
#rnd <- runif(dim(rc)[1])
#rc <- rc[rnd<.3,]

### IMPORT DIPUTADO INFO (PREPARED IN EXCEL): MOST LIKELY REDUNDANT BEC rc60.RData HAS IT
#setwd(paste(workdir, "/diputados", sep=""))
#dipdat <- read.csv("dip60.csv", header=TRUE)
#setwd(workdir)

### UTIL PARA SABER QUIEN ES QUIEN
#ord	n	nom	id	part
#1	1	Armend?riz Garc?a Pedro	ags01p	pan
#2	501	Lucio Ochoa Patricia	ags01s	pan
#3	2	Ruiz Velazco De Lira Ernesto	ags02p	pan
#4	502	Becerril Alba Jimena	ags02s	pan
#5	3	Medina Mac?as Alma Hilda	ags03p	pan
#6	503	Huerta Enr?quez Juan Manuel	ags03s	pan
#7	301	D?vila Garc?a Francisco	agsrp01p	pan
#8	801	Valdez Parga Tania Lorena	agsrp01s	pan
#9	302	Hurtado P?rez Nelly Asunci?n	agsrp02p	pan
#10	802	Caballero Sig?enza Vicente Vladimir	agsrp02s	pan
#11	303	Ortega Mart?nez Antonio	agsrp03p	prd
#12	803	Gaxiola F?lix Marcelo	agsrp03s	prd
#13	304	Mart?nez Rodr?guez Lorena	agsrp04p	pri
#14	804	Femat Flores Jos? Alfredo	agsrp04s	pri
#15	305	Olivares Ventura H?ctor Hugo	agsrp05p	pri
#16	805	Sandoval ?vila Alejandro	agsrp05s	pri
#17	306	Luna Rodr?guez Silvia	agsrp06p	panal
#18	806	De La Pe?a Cepeda Dayana Maricela	agsrp06s	panal
#19	4	Rueda G?mez Francisco	bc01p	pan
#20	504	Contreras Covarrubias Adelaida	bc01s	pan
#21	5	Manuell G?mez Angulo Dolores De Mar?a	bc02p	pan
#22	505	Arjona Ben?tez Jes?s Armando	bc02s	pan
#23	6	Ramos Covarrubias H?ctor Manuel	bc03p	pan
#24	506	L?pez Peralta Estefana	bc03s	pan
#25	7	Franco C?zares Ricardo	bc04p	pan
#26	507	Sotelo Miranda Patricia Lorena	bc05s	pan
#27	8	Valladolid Rodr?guez Antonio	bc06p	pan
#28	508	Arana Cruz Georgina Er?ndira	bc06s	pan
#29	9	Enr?quez Mart?nez Luis Rodolfo	bc07p	pan
#30	509	Cardona Benav?dez Alma X?chitl	bc07s	pan
#31	10	Paredes Rodr?guez Francisco Javier	bc08p	pan
#32	510	Araujo Villegas Guadalupe Armida	bc08s	pan
#33	11	Rinc?n Vargas Mirna Cecilia	bc09p	pan
#34	511	L?pez Hern?ndez Jos? Ernesto	bc09s	pan
#35	307	Maldonado Gonz?lez David	bcrp01p	pan
#36	807	Guti?rrez Espinosa Yolanda	bcrp01s	pan
#37	308	Torres Torres Carlos Alberto	bcrp02p	pan
#38	808	S?nchez Meza Mar?a Luisa	bcrp02s	pan
#39	309	Ayala Almeida Joel	bcrp03p	pri
#40	809	De La Torre Hern?ndez Mario Ram?n	bcrp03s	pri
#41	12	Orci Mart?nez Juan Adolfo	bcs01p	prd
#42	512	Perpuli Pag?s Fidencio	bcs01s	prd
#43	13	Liz?rraga Peraza V?ctor Manuel	bcs02p	prd
#44	513	Trevi?o Garza Graciela	bcs02s	prd
#45	310	Larregui Nagel Erika	bcsrp01p	pvem
#46	810	Salgado Amador Manuel Salvador	bcsrp01s	pvem
#47	14	M?ndez Lanz V?ctor Manuel	cam01p	pri
#48	514	Bern?s Chan Blanca Rosalina	cam01s	pri
#49	15	Mart?nez Rocha Arturo	cam02p	pri
#50	515	Moreno Chamiz Nadia Lizet	cam02s	pri
#51	311	Nordhausen Gonz?lez Jorge Rub?n	camrp01p	pan
#52	811	Guerra S?nchez Mar?a De Jes?s	camrp01s	pan
#53	312	Sansores San Rom?n Layda Elena	camrp02p	conve
#54	812	Ostoa Ortega An?bal	camrp02s	conve
#55	16	Garc?a Reyes ?ngel Humberto	coa01p	pan
#56	516	Aguayo P?rez Bertha Alicia	coa01s	pan
#57	17	Guerrero Garc?a Javier	coa02p	pri
#58	517	Garza Herrera Santos	coa02s	pri
#59	18	Rivero Rivero Rolando	coa03p	pan
#60	518	Herrera Esparza Mar?a del Rosario	coa03s	pan
#61	19	Abramo Masso Yerico	coa04p	pri
#62	519	Antunes Flores Jos? Antonio	coa04s	pri
#63	20	Bracho Gonz?lez Carlos Augusto	coa05p	pan
#64	520	Rodr?guez Vega Elizabeth	coa05s	pan
#65	21	De Le?n Tello Jes?s	coa06p	pan
#66	521	Medrano Echeverr?a Florentina	coa06s	pan
#67	22	Mohamar Dainitin Oscar Miguel	coa07p	pan
#68	522	Marrufo L?pez Virginia Elena	coa07s	pan
#69	313	Fern?ndez Ugarte Ma. del Carmen	coarp01p	pan
#70	813	Hern?ndez N??ez Elia	coarp01s	pan
#71	314	Flores Morfin Jes?s Vicente	coarp02p	pan
#72	814	Betancourt V?zquez Mar?a del Socorro	coarp02s	pan
#73	315	Mart?nez Valero Dora Alicia	coarp03p	pan
#74	815	Botello Maldonado Cosme Iv?n	coarp03s	pan
#75	316	Carbajal Tejada Rogelio	coarp04p	pan
#76	816	Montiel Luis Lariza	coarp04s	pan
#77	317	D?vila Esquivel Humberto	coarp05p	panal
#78	817	Delgado Hern?ndez Regina Mahelet	coarp05s	panal
#79	23	C?rdenas S?nchez Esmeralda	col01p	pan
#80	523	Mart?nez Valencia Ferdinando Enrique	col01s	pan
#81	24	Ochoa L?pez Nabor	col02p	indep
#82	524	Cervantes Mart?nez Ana Mar?a	col02s	indep
#83	318	Torres Herrera V?ctor Manuel	colrp01p	pan
#84	818	Pe?a Valencia Gicela	colrp01s	pan
#85	319	Ochoa Gonz?lez Arnoldo	colrp02p	pri
#86	819	V?zquez G?ngora Alejandro Canek	colrp02s	pri
#87	320	Hern?ndez Valad?s Delio	colrp03p	asd
#88	820	Iglesias Bravo Antelmo	colrp03s	asd
#89	25	Gebhardt Garduza Yary del Carmen	cps01p	pri
#90	525	Estrada ?lvaro Mariano	cps01s	pri
#91	26	Ortiz del Carpio V?ctor	cps02p	pri
#92	526	Hern?ndez P?rez Lorenzo	cps02s	pri
#93	27	D?az Sol?rzano Elmar Darinel	cps03p	pri
#94	527	Chulin Jim?nez Pedro	cps03s	pri
#95	28	Carballo Bustamante Andr?s	cps04p	pri
#96	528	Zenteno P?rez Nelly Mar?a	cps04s	pri
#97	29	Lescieur Talavera Jorge Mario	cps05p	pri
#98	529	P?rez Dom?nguez Lorenzo	cps05s	pri
#99	30	Narc?a ?lvarez H?ctor	cps06p	prd
#100	530	Hern?ndez P?rez Mario	cps06s	prd
#101	31	G?lvez Rodr?guez Fernel Arturo	cps07p	prd
#102	531	Antonio Vel?zquez Mar?a Ang?lica	cps07s	prd
#103	32	Cordero Alfonzo Arnulfo El?as	cps08p	pri
#104	532	Cancino Herrera Ali	cps08s	pri
#105	33	Morales V?zquez Carlos Orsoe	cps09p	prd
#106	533	De Le?n Villard Kalyanamaya	cps09s	prd
#107	34	Ramos Castellanos Mart?n	cps10p	prd
#108	534	Navarro Rivera Luis De Jes?s	cps10s	prd
#109	35	Herrera Sol?s Anuario Luis	cps11p	pt
#110	535	Villagr?n P?rez Luis	cps11s	pt
#111	36	D?az Athi? Antonio	cps12p	pri
#112	536	Vald?s Gal?n Mar?a De Lourdes	cps12s	pri
#113	321	D?az Gordillo Martha Cecilia	cpsrp01p	pan
#114	821	D?az D?az Javier	cpsrp01s	pan
#115	322	Gonz?lez Betancourt Jorge Justiniano	cpsrp02p	pan
#116	822	Mois?s Ram?rez Nydia Jeanette	cpsrp02s	pan
#117	323	Escand?n Cadenas Rutilio Cruz	cpsrp03p	prd
#118	823	Madariaga Rivera Rub?n	cpsrp03s	prd
#119	324	Matus Toledo Holly	cpsrp04p	prd
#120	824	Vicente V?zquez Sa?l	cpsrp04s	prd
#121	325	Madrid Tovilla Areli	cpsrp05p	pri
#122	825	Quintero Mateos Armando	cpsrp05s	pri
#123	326	Peregrino Garc?a Abundio	cpsrp06p	pt
#124	826	P?rez Flores Neftal? Ignacio	cpsrp06s	pt
#125	37	Serrano Escobar Enrique	cua01p	pri
#126	537	Terrazas Porras Adriana	cua01s	pri
#127	38	Merodio Reza Lilia Guadalupe	cua02p	pri
#128	538	Quevedo Valles Oscar Alberto	cua02s	pri
#129	39	P?rez Cu?llar Cruz	cua03p	pan
#130	539	C?rdenas de la Torre Miriam Gabriela	cua03s	pan
#131	40	Valencia de los Santos V?ctor Leopoldo	cua04p	pri
#132	540	Fuentes T?llez Octavio	cua04s	pri
#133	41	Gonz?lez Ruiz Felipe	cua05p	pan
#134	541	Garc?a Gonz?lez Elsa	cua05s	pan
#135	42	Flores Dom?nguez Emilio Ram?n Ramiro	cua06p	pan
#136	542	Baray Trujillo Rosa Mar?a	cua06s	pan
#137	43	Beltr?n Montes Israel	cua07p	pri
#138	543	Renter?a Wong Alberto	cua07s	pri
#139	44	Reyes L?pez Carlos Armando	cua08p	pan
#140	544	Silva Muela Graciela Alejandra	cua08s	pan
#141	45	Duarte J?quez C?sar	cua09p	pri
#142	545	Garfio Pacheco Javier Alfonso	cua09s	pri
#143	327	Campos Galv?n Mar?a Eugenia	cuarp01p	pan
#144	827	Nieves Robinson Bours Luis Ernesto	cuarp01s	pan
#145	328	Limas Frescas Mar?a Soledad	cuarp02p	pan
#146	828	Mondrag?n Quintana Juan Carlos	cuarp02s	pan
#147	329	Aguilar Jim?nez Rub?n	cuarp03p	pt
#148	829	Gil Herrera Matilde	cuarp03s	pt
#149	330	G?mez Pasillas Jacinto	cuarp04p	panal
#150	830	Navarrete Ordo?ez Ra?l Armando	cuarp04s	panal
#151	46	Lozano Lozano Andr?s	df01p	prd
#152	546	Pi?a Calva Carlos	df01s	prd
#153	47	Gonz?lez Garza Javier	df02p	prd
#154	547	Gonz?lez Rosales Federico	df02s	prd
#155	48	Pacheco Llanes Ram?n F?lix	df03p	prd
#156	548	Pineda D?az Adolfo	df03s	prd
#157	49	Alonso Flores Lourdes	df04p	prd
#158	549	Ram?rez G?mez Leticia	df04s	prd
#159	50	Contreras Juli?n Maricela	df05p	prd
#160	550	Casta?eda Pacheco Araceli X?chitl	df05s	prd
#161	51	Torres Baltazar Mar?a Elena	df06p	prd
#162	551	L?pez Lira Adriana	df06s	prd
#163	52	Guerra Ochoa Juan Nicasio	df07p	prd
#164	552	Neyra Medel Aureliano Jorge	df07s	prd
#165	53	Barreiro P?rez Armando	df08p	prd
#166	553	Aguilera y L?pez Salom?n	df08s	prd
#167	54	Montalvo Rojas Victorio Rub?n	df09p	prd
#168	554	Hern?ndez Cruz Armando	df09s	prd
#169	55	Gonz?lez Mart?nez Mar?a Gabriela	df10p	pan
#170	555	M?rquez Argueta Ramiro	df10s	pan
#171	56	Garc?a Rodr?guez V?ctor Hugo	df11p	prd
#172	556	Gonz?lez Huerta Netzahualc?yotl	df11s	prd
#173	57	Su?rez del Real y Aguilera Jos? Alfonso	df12p	prd
#174	557	Fern?ndez L?pez Jos?	df12s	prd
#175	58	Trejo P?rez Pablo	df13p	prd
#176	558	Mart?nez Barrientos Juan Leonardo	df13s	prd
#177	59	Ch?vez Garc?a Higinio	df14p	prd
#178	559	Cabrera Mortera Mar?a del Carmen	df14s	prd
#179	60	Minjares Jim?nez Jos? Manuel	df15p	pan
#180	560	Denegre Vaught Ram?rez Rosaura Virginia	df15s	pan
#181	61	Batres Guadarrama Valentina Valia	df16p	prd
#182	561	L?pez Mondrag?n Juan	df16s	prd
#183	62	Alavez Ruiz Aleida	df17p	prd
#184	562	Mart?nez Blanca Estela	df17s	prd
#185	63	Mendoza Arellano David	df18p	prd
#186	563	Figueroa Acu?a Modesto	df18s	prd
#187	64	Oliva Fragoso Silvia	df19p	prd
#188	564	Macedo Escart?n Miguel ?ngel	df19s	prd
#189	65	Morales S?nchez Efra?n	df20p	prd
#190	565	Loza Sosa Amelia	df20s	prd
#191	67	S?nchez Camacho Alejandro	df21p	prd
#192	567	Alvarado Gonz?lez David	df21s	prd
#193	68	Varela L?pez V?ctor Gabriel	df22p	prd
#194	568	Ortiz G?mez Mar?a Susana	df22s	prd
#195	69	Pedrozo Castillo Adri?n	df23p	prd
#196	569	Zambrano Reyes J. De Jes?s	df23s	prd
#197	70	Villanueva Albarr?n Gerardo	df24p	prd
#198	570	Gil Ram?rez Mois?s	df24s	prd
#199	71	Solares Ch?vez Miguel ?ngel	df25p	prd
#200	571	Becerra Cornejo Ram?n	df25s	prd
#201	72	Guti?rrez Calzadilla Jos? Luis	df26p	prd
#202	572	Gayosso Rodr?guez Oscar	df26s	prd
#203	73	Flores Salazar Guadalupe Socorro	df27p	prd
#204	573	Cabrera Jim?nez Hugo	df27s	prd
#205	331	?vila Mayo Obdulio	dfrp01p	pan
#206	831	P?rez Toxqui S?nchez Sandra Esther	dfrp01s	pan
#207	332	Fern?ndez Cabrera Adri?n	dfrp02p	pan
#208	832	Hinojosa C?spedes Adriana De Lourdes	dfrp02s	pan
#209	333	Lujano Nicol?s Christian Mart?n	dfrp03p	pan
#210	833	?lvarez Mac?as Patricia	dfrp03s	pan
#211	334	Gonz?lez Roaro Benjam?n Ernesto	dfrp04p	pan
#212	834	Ramos Barrag?n Ruth	dfrp04s	pan
#213	335	Noriega Blanco Vigil Mar?a Elena	dfrp05p	pan
#214	835	Abascal Olascoaga Rodrigo	dfrp05s	pan
#215	336	Cruz Santiago Claudia Lilia	dfrp06p	prd
#216	836	S?nchez N?stor Martha	dfrp06s	prd
#217	337	Jacques y Medina Jos?	dfrp07p	prd
#218	837	L?pez Dur?n Alfredo	dfrp07s	prd
#219	338	Ortiz Magall?n Rosario Ignacia	dfrp08p	prd
#220	838	Quintero Mart?nez Mar?a Columba	dfrp08s	prd
#221	338	S?nchez Camacho David	dfrp09p	prd
#222	839	Torres Osorno Luis Antonio	dfrp09s	prd
#223	340	Soto Ramos Faustino	dfrp10p	prd
#224	840	S?nchez Valdez Eva Angelina	dfrp10s	prd
#225	341	Zavaleta Salgado Ruth	dfrp11p	prd
#226	841	Segura Trejo Elena Edith	dfrp11s	prd
#227	342	Pag?s Llergo Rebollar Mar?a Beatriz	dfrp12p	pri
#228	842	Monta?o Resa Eduardo Agust?n	dfrp12s	pri
#229	343	S?nchez Hern?ndez Eduardo	dfrp13p	pri
#230	843	Leal Hern?ndez Eduardo	dfrp13s	pri
#231	344	Cervantes Andrade Ra?l	dfrp14p	pri
#232	844	Cerezo Torres Roberto Efr?n	dfrp14s	pri
#233	345	Chanona Burguete Alejandro	dfrp15p	conve
#234	845	Iturbe Flores H?ctor	dfrp15s	conve
#235	346	Tagle Mart?nez Martha Ang?lica	dfrp16p	conve
#236	846	Tapia Latisnere Paulino Gerardo	dfrp16s	conve
#237	347	Velasco Oliva Cuauht?moc	dfrp17p	conve
#238	847	Rosado y Garc?a Antonio	dfrp17s	conve
#239	348	Lavara Mej?a Gloria	dfrp18p	pvem
#240	848	Carre?n Valencia Javier Iv?n	dfrp18s	pvem
#241	349	Ram?rez Zollino Andrea Geraldine	dfrp19p	pvem
#242	849	Gonz?lez Mart?nez Jorge Emilio	dfrp19s	pvem
#243	350	Arreola Ortega Pablo Leopoldo	dfrp20p	pt
#244	850	Aparicio Barrios Arturo	dfrp20s	pt
#245	351	Luna Becerril Blanca	dfrp21p	panal
#246	851	Sevilla ?lvarez Karime Iyari	dfrp21s	panal
#247	352	Jim?nez God?nez Miguel ?ngel	dfrp22p	panal
#248	852	P?rez Bola?os Ana Elisa	dfrp22s	panal
#249	353	Arvizu Rivas Aida Marina	dfrp23p	asd
#250	853	Rodr?guez Haro Lissette Jessica	dfrp23s	asd
#251	354	Pedro Cort?s Santiago Gustavo	dfrp24p	asd
#252	854	R?os V?zquez Alfonso Primitivo	dfrp24s	asd
#253	74	Ben?tez Ojeda Luis Enrique	dgo01p	pri
#254	574	Vida?a Hern?ndez Jos? Roberto	dgo01s	pri
#255	75	Herrera Ale Juana Leticia	dgo02p	pri
#256	575	Uribe Rodr?guez Sergio	dgo02s	pri
#257	76	Escajeda Jim?nez Jos? Rub?n	dgo03p	pri
#258	576	Quiroz Ontiveros Mario	dgo03s	pri
#259	77	Salum del Palacio Jorge Alejandro	dgo04p	pan
#260	577	Jim?nez Delgado Silvia Patricia	dgo04s	pan
#261	355	Castro Mu?oz Juan De Dios	dgorp01p	pan
#262	855	Galv?n Valles Rosa Elena	dgorp01s	pan
#263	356	Aguilar Sol?s Samuel	dgorp02p	pri
#264	856	De Lamadrid T?llez Jos? Luis	dgorp02s	pri
#265	357	Aispuro Torres Jos? Rosas	dgorp03p	pri
#266	857	I?iguez Ibarra Gustavo	dgorp03s	pri
#267	358	Qui?ones Canales Lourdes	dgorp04p	pri
#268	858	Monta?o Rosa Mar?a	dgorp04s	pri
#269	359	Ar?valo Gonz?lez Jos? Antonio	dgorp05p	pvem
#270	859	Ochoa Cervantes Jos? Manuel	dgorp05s	pvem
#271	78	Murillo Flores Francisco Javier	gua01p	pan
#272	578	Huerta Arroyo Ra?l Alberto	gua01s	pan
#273	79	Stefanonni Mazzocco Mart?n	gua02p	pan
#274	579	Garc?a Luevano Rebeca Lorena	gua02s	pan
#275	80	Oviedo Oviedo Ernesto	gua03p	pan
#276	580	Enr?quez Van Der Kam Ariadna	gua03s	pan
#277	81	Arenas Guzm?n Margarita	gua04p	pan
#278	581	Garc?a Olvera Marcos Amador	gua04s	pan
#279	82	Ram?rez Barba Ector Jaime	gua05p	pan
#280	582	S?nchez Junquera Fabiola	gua05s	pan
#281	83	Rodr?guez Vizcarra Vel?zquez Adriana	gua06p	pan
#282	583	Hern?ndez Hern?ndez J. Ram?n	gua06s	pan
#283	84	Verd?n Salda?a Jaime	gua07p	pan
#284	584	Ochoa Casanova Alma Leticia	gua07s	pan
#285	85	Vega Corona Antonio	gua08p	pan
#286	585	Ar?valo Ram?rez Laura Jessica	gua08s	pan
#287	86	Cuen Garibi Marcela	gua09p	pan
#288	586	Gonz?lez Garc?a Francisco	gua09s	pan
#289	87	Torres G?mez Artemio	gua10p	pan
#290	587	Jim?nez Cruz Josefina	gua10s	pan
#291	88	Landeros Gonz?lez Ram?n	gua11p	pan
#292	588	Rodr?guez Preciado Ma. Magdalena	gua11s	pan
#293	89	L?pez Silva Rub? Laura	gua12p	pan
#294	589	Rodr?guez Guevara Carlos	gua12s	pan
#295	90	Lemus Mu?oz Ledo Ram?n Ignacio	gua13p	pan
#296	590	Garc?a Hern?ndez Liliana	gua13s	pan
#297	91	Malag?n R?os Mart?n	gua14p	pan
#298	591	Soto Escamilla Katya Cristina	gua14s	pan
#299	360	Hern?ndez N??ez Elia	guarp01p	pan
#300	860	Mu?oz Cortez Mois?s Felipe	guarp01s	pan
#301	361	Magall?n Arceo Leonardo Melesio	guarp02p	pan
#302	861	Salazar L?pez Alejandra del Carmen	guarp02s	pan
#303	362	Ortega Mart?nez Ma. del Pilar	guarp03p	pan
#304	862	Narv?ez Mart?nez Jos? Ricardo	guarp03s	pan
#305	363	L?pez Torres Ma. Soledad	guarp04p	prd
#306	863	Muza Bernal Jaime Javier	guarp04s	prd
#307	364	Chaurand Arzate Carlos	guarp05p	pri
#308	864	Guevara Cobos Luis Alejandro	guarp05s	pri
#309	365	L?pez Ram?rez Sergio Augusto	guarp06p	pvem
#310	865	Manrique Guevara Beatriz	guarp06s	pvem
#311	366	Sol?s Parga Rodolfo	guarp07p	pt
#312	866	D?az L?pez Rosalinda	guarp07s	pt
#313	92	Torres Garc?a Daniel	gue01p	prd
#314	592	Pe?a Damacio Efra?n	gue01s	prd
#315	93	Brito Gonz?lez Modesto	gue02p	prd
#316	593	Barrera Rodr?guez Rafael Celio	gue02s	prd
#317	94	Campos Aburto Amador	gue03p	prd
#318	594	Catarino Crisp?n Secundino	gue03s	prd
#319	95	Almonte Borja Ram?n	gue04p	prd
#320	595	Rabad?n Delgado Miguel ?ngel	gue04s	prd
#321	96	Aguirre Alcaide V?ctor	gue05p	prd
#322	596	Huerta L?pez Jos? Misael	gue05s	prd
#323	97	Mat?as Alonso Marcos	gue06p	pt
#324	597	Morelos Estrada Claudio Rafael	gue06s	pt
#325	98	S?nchez Barrios Carlos	gue07p	prd
#326	598	Klimek Alcaraz Octavio Adolfo	gue07s	prd
#327	99	Romero Guti?rrez Odil?n	gue08p	prd
#328	599	Jacobo Valle Jos?	gue08s	prd
#329	100	Flores Maldonado C?sar	gue09p	prd
#330	600	Pi?a Garibay Miguel ?ngel	gue09s	prd
#331	367	Arizmendi Uribe Efra?n	guerp01p	pan
#332	867	Alan?s Dom?nguez Mar?a Teresa	guerp01s	pan
#333	368	Sandoval Ram?rez Cuauht?moc	guerp02p	prd
#334	868	Ad?n Tabares Juan	guerp02s	prd
#335	369	Zazueta Aguilar Jes?s Humberto	guerp03p	prd
#336	869	Galindo Hern?ndez Sergio Iv?n	guerp03s	prd
#337	370	Castellanos Hern?ndez F?lix	guerp04p	conve
#338	870	Ortega Cort?s Zenaida	guerp04s	conve
#339	101	Guerrero Ju?rez Joel	hgo01p	pri
#340	601	Azuara Castel?n Mar?a del Carmen	hgo01s	pri
#341	102	Ram?rez Mart?nez Jos? Edmundo	hgo02p	pri
#342	602	Hern?ndez Garc?a Leticia	hgo02s	pri
#343	103	Hern?ndez Hern?ndez Sergio	hgo03p	prd
#344	603	Ram?rez P?rez Alejandro	hgo03s	prd
#345	104	Vega Ortiz Mar?a Oralia	hgo04p	pri
#346	604	Rivero Acosta Miguel	hgo04s	pri
#347	105	Moctezuma Pereda Fernando	hgo05p	pri
#348	605	Narv?ez Bravo Hilda Areli	hgo05s	pri
#349	106	Ludlow Kuri Lorenzo Daniel	hgo06p	pan
#350	606	Del Villar Sosa Sonia Leslie	hgo06s	pan
#351	107	Pe?a S?nchez Miguel ?ngel	hgo07p	prd
#352	607	Ortega Luna Abel	hgo07s	prd
#353	371	Peyrot Sol?s Marco Antonio	hgorp01p	pan
#354	871	Rivera Villanueva Erick Marte	hgorp01s	pan
#355	372	Pedraza Ch?vez Isidro	hgorp02p	prd
#356	872	Fern?ndez Hern?ndez Jos? Cuauht?moc	hgorp02s	prd
#357	373	Sosa Castel?n Gerardo	hgorp03p	pri
#358	873	Aguilar Ch?vez Mart?n Julio	hgorp03s	pri
#359	107	Mac?as Zambrano Gustavo	jal01p	pan
#360	607	Carrillo Sandoval Fortino	jal01s	pan
#361	108	Romo Jim?nez Martha Ang?lica	jal02p	pan
#362	608	Romo Ch?vez Iv?n De Jes?s	jal02s	pan
#363	109	Mu?oz Serrano Jos? Antonio	jal03p	pan
#364	609	J?uregui G?mez Fabiola	jal03s	pan
#365	110	Quintero Bello Jorge	jal04p	pan
#366	610	Castillo ?vila Beatriz Adriana	jal04s	pan
#367	111	Curiel Preciado Leobardo	jal05p	pan
#368	611	Rodr?guez Ramos Mar?a Cristina	jal05s	pan
#369	112	Arellano Arellano Joel	jal06p	pan
#370	612	Hidalgo Mor?n Juan Manuel	jal06s	pan
#371	113	S?nchez Gil Carlos Ren?	jal07p	pan
#372	613	Guti?rrez Razo Jos? Guadalupe	jal07s	pan
#373	114	Monraz Ibarra Miguel ?ngel	jal08p	pan
#374	614	Rodr?guez Varillas Luis Alberto	jal08s	pan
#375	115	Montes S?nchez Fabi?n Fernando	jal09p	pan
#376	615	Flores Jim?nez Humberto	jal09s	pan
#377	116	Borboa Becerra Omar Antonio	jal10p	pan
#378	616	Guti?rrez Aguilar Miguel ?ngel	jal10s	pan
#379	117	Lizaola De La Torre Alonso Manuel	jal11p	pan
#380	617	Chi?as Flores Te?filo	jal11s	pan
#381	118	Moreno ?lvarez Mario Eduardo	jal12p	pan
#382	618	Parra Jacobo H?ctor Manuel	jal12s	pan
#383	119	Solano Mu?oz Jos? De Jes?s	jal13p	pan
#384	619	Basulto God?nez J. Jes?s	jal13s	pan
#385	120	Guerrero Torres Jos? Gildardo	jal14p	pan
#386	620	Lomel? Famoso Ricardo	jal14s	pan
#387	121	Amezola Fonceca Gerardo	jal15p	pan
#388	621	Flores Castellanos Gabriel	jal15s	pan
#389	122	Plascencia Alonso Francisco Javier	jal16p	pan
#390	622	Aceves Reynoso Jos? Antonio	jal16s	pan
#391	123	Gudi?o Ortiz Francisco Javier	jal17p	pan
#392	623	Mendoza Guti?rrez Manuel	jal17s	pan
#393	124	Morales Ramos Jos? Nicol?s	jal18p	pan
#394	624	Llamas Flores Nolverto	jal18s	pan
#395	125	Barajas del Toro Salvador	jal19p	pri
#396	625	Mu?oz Garc?a Crisanta	jal19s	pri
#397	374	Mendoza Morales Luc?a Susana	jalrp01p	pan
#398	874	Huerta Gonz?lez V?ctor Hugo	jalrp01s	pan
#399	375	Morgan Franco Roc?o del Carmen	jalrp02p	pan
#400	875	Mu?oz Vargas Humberto	jalrp02s	pan
#401	376	Padilla Orozco Ra?l Alejandro	jalrp03p	pan
#402	876	P?rez Camarena Carmen Luc?a	jalrp03s	pan
#403	377	Rodr?guez Jim?nez Ricardo	jalrp04p	pan
#404	877	Hern?ndez Boj?rquez Rodolfo Olimpo	jalrp04s	pan
#405	378	Salazar Madera Mario Alberto	jalrp05p	pan
#406	878	G?mez Hern?ndez Beatriz Adriana	jalrp05s	pan
#407	379	Bravo Padilla Itzc?atl Tonatiuh	jalrp06p	prd
#408	879	Garc?a G?mez Constanza Mayra	jalrp06s	prd
#409	380	Barba Hern?ndez Alfredo	jalrp07p	pri
#410	880	Hern?ndez Rodr?guez Juan	jalrp07s	pri
#411	381	Flores Sandoval Patricio	jalrp08p	pri
#412	881	Mart?nez Ortega Jes?s Francisco	jalrp08s	pri
#413	382	Padilla Guti?rrez H?ctor	jalrp09p	pri
#414	882	Zazueta F?lix Marco Antonio	jalrp09s	pri
#415	383	Rodr?guez Ram?rez Bertha Yolanda	jalrp10p	pri
#416	883	Fr?as Valenzuela Mario Ignacio	jalrp10s	pri
#417	384	Chozas y Chozas Olga Patricia	jalrp11p	pvem
#418	884	Rodr?guez Luis Alejandro	jalrp11s	pvem
#419	126	Alc?ntara N??ez Jes?s Sergio	mex01p	pri
#420	626	Mendoza Mondrag?n Mar?a Luisa	mex01s	pri
#421	127	Abad De Jes?s Juan	mex02p	conve
#422	627	Romero Morales Jos? Meinardo	mex02s	conve
#423	128	C?rdenas Monroy Oscar Gustavo	mex03p	pri
#424	628	Almanza Monroy Fidel	mex03s	pri
#425	129	Acosta D?vila Constantino	mex04p	pan
#426	629	Galv?n Rivas Erika	mex04s	pan
#427	130	Saavedra Coronel Jos? Antonio	mex05p	prd
#428	630	P?rez Luna Armando	mex05s	prd
#429	131	L?pez Becerra Santiago	mex06p	prd
#430	631	Escalona Cort?s Mauro	mex06s	prd
#431	132	Arredondo Ibarra Salvador	mex07p	pan
#432	632	Carvajal Adame Mar?a del Carmen	mex07s	pan
#433	133	Mart?nez Mart?nez Francisco	mex08p	prd
#434	633	Gonz?lez Miranda Facundo	mex08s	prd
#435	134	G?mez Lugo Elda	mex09p	pri
#436	634	Blanco Rodr?guez Eduardo	mex09s	pri
#437	135	Mart?nez Vargas Octavio	mex10p	prd
#438	635	Flores Mart?nez Antonio	mex10s	prd
#439	136	Ruiz S?nchez Salvador	mex11p	prd
#440	636	Jim?nez Trejo Jos? Juan	mex11s	prd
#441	137	Ramos Becerril Rafael Pl?cido	mex12p	conve
#442	637	Vel?zquez Lara Alejandro	mex12s	conve
#443	138	Alva Olvera Maribel Luisa	mex13p	prd
#444	638	Sim?n Hern?ndez Juan Antonio	mex13s	prd
#445	139	Madrazo Lim?n Carlos	mex14p	pan
#446	639	Mart?nez V?zquez Mar?a del Refugio	mex14s	pan
#447	140	Landero Guti?rrez Alejandro	mex15p	pan
#448	640	L?pez Espinosa Patricia Josefina	mex15s	pan
#449	141	P?rez Cruz Raciel	mex16p	prd
#450	641	Arenas S?nchez H?ctor	mex16s	prd
#451	142	Mart?nez Padilla Hugo Eduardo	mex17p	prd
#452	642	Alcaraz Castillo Daniel	mex17s	prd
#453	143	S?nchez Ju?rez Claudia	mex18p	pan
#454	643	Franco Valencia Mario	mex18s	pan
#455	144	Del Toro Mario Enrique	mex19p	prd
#456	644	Guerrero C?rdenas Bulmaro	mex19s	prd
#457	145	Zepeda Hern?ndez Mart?n	mex20p	prd
#458	645	Parra Elizarraras Alfonso	mex20s	prd
#459	146	Olvera Higuera Edgar Armando	mex21p	pan
#460	646	Pati?o S?nchez Mar?a Eugenia	mex21s	pan
#461	147	Alcalde Virgen Mois?s	mex22p	pan
#462	647	Reyes Garc?a Mar?a Isabel	mex22s	pan
#463	148	Col?n Guadarrama Mar?a Mercedes	mex23p	pri
#464	648	S?nchez Garc?a H?ctor Jaime	mex23s	pri
#465	149	Godoy C?rdenas Jorge	mex24p	conve
#466	649	G?mez Guerrero Francisco Jos?	mex24s	conve
#467	150	L?pez Rojas Alberto	mex25p	prd
#468	650	P?rez S?nchez Heriberto	mex25s	prd
#469	151	Enr?quez Flores Armando	mex26p	pan
#470	651	Ovalles Castro Mar?a Aurora	mex26s	pan
#471	152	Maawad Robert Luis Xavier	mex27p	pan
#472	652	Ortiz Mart?nez De Kores Gabriela	mex27s	pan
#473	153	Mu?oz Serna Rogelio	mex28p	pri
#474	653	Col?n Castro Virginia	mex28s	pri
#475	154	Ulloa P?rez Emilio	mex29p	prd
#476	654	Rojas Carmona Sergio	mex29s	prd
#477	155	Bautista Bravo Alliet Mariana	mex30p	prd
#478	655	Flores Casta?eda Petra	mex30s	prd
#479	156	De La Rosa Garc?a Juan Hugo	mex31p	prd
#480	656	Barrera Hern?ndez Juan Carlos	mex31s	prd
#481	157	Luna Mungu?a Alma Lilia	mex32p	prd
#482	657	Uraga Pe?a Urbano Israel	mex32s	prd
#483	158	Espejel Lazcano Jaime	mex33p	prd
#484	658	Rivas Robles Mart?n Valdemar Octavio	mex33s	prd
#485	159	Gonz?lez Mor?n Mart?n Oscar	mex34p	pan
#486	659	Macedo Dom?nguez Mar?a Magdalena	mex34s	pan
#487	160	Olivares Monterrubio Alejandro	mex35p	pri
#488	660	Guti?rrez Ansaldo Edith Guadalupe	mex35s	pri
#489	161	Villa Villa Isael	mex36p	pri
#490	661	Maya G?mez Teresita	mex36s	pri
#491	162	Santos Arreola Francisco Javier	mex37p	prd
#492	662	S?nchez Torres Ricardo	mex37s	prd
#493	163	Arreola Calder?n Juan Dar?o	mex38p	prd
#494	663	Escamilla Gonz?lez Raymundo	mex38s	prd
#495	164	San Mart?n Hern?ndez Juan Manuel	mex39p	prd
#496	664	De La Rosa Mil?n Pedro	mex39s	prd
#497	165	Victoria Alva Juan	mex40p	pan
#498	665	Garc?a Gonz?lez Gloria	mex40s	pan
#499	385	Del Toro del Villar Tom?s	mexrp01p	pan
#500	885	Luna Islas Claudia Lilia	mexrp01s	pan
#501	386	G?mez Leyva Silvio	mexrp02p	pan
#502	886	Berist?in Enr?quez Luc?a	mexrp02s	pan
#503	387	Parra Noriega Luis Gustavo	mexrp03p	pan
#504	887	Mart?nez Bernal Blanca Margarita	mexrp03s	pan
#505	388	Rojas Hern?ndez Laura Ang?lica	mexrp04p	pan
#506	888	Flores Olvera Pedro	mexrp04s	pan
#507	389	S?nchez Dom?nguez Alejandro	mexrp05p	pan
#508	889	S?nchez Garc?a Mar?a De Los ?ngeles	mexrp05s	pan
#509	390	Sandoval Mungu?a Juan Manuel	mexrp06p	pan
#510	890	S?nchez P?rez Gilberto Alejandro	mexrp06s	pan
#511	391	Alfredo Rivadeneyra Hern?ndez	mexrp07p	pan
#512	891	P?rez de Tejada Romero Diana Carolina	mexrp07s	pan
#513	392	Hern?ndez Manzanares Javier	mexrp08p	prd
#514	892	Barrag?n V?lez Juan Carlos	mexrp08s	prd
#515	393	Leyva Pi??n Ana Yurixi	mexrp09p	prd
#516	893	Reyes Montiel Claudia	mexrp09s	prd
#517	394	S?nchez Jim?nez Luis	mexrp10p	prd
#518	894	Salinas P?rez Josefina	mexrp10s	prd
#519	395	Camacho Quiroz C?sar	mexrp11p	pri
#520	895	Valencia Mart?nez Alejandro	mexrp11s	pri
#521	396	Gonz?lez Calder?n Martha Hilda	mexrp12p	pri
#522	896	Jaimes Archundia Erik Iv?n	mexrp12s	pri
#523	397	C?rdenas M?rquez El?as	mexrp13p	conve
#524	897	Moguel Ballado Oscar Octavio	mexrp13s	conve
#525	398	Vald?s Ch?vez Ram?n	mexrp14p	conve
#526	898	Arredondo Ozuna H?ctor	mexrp14s	conve
#527	399	Samperio Monta?o Juan Ignacio	mexrp15p	conve
#528	899	Ochoa Mej?a Mar?a Teresa Rosaura	mexrp15s	conve
#529	400	Notholt Guerrero Alan	mexrp16p	pvem
#530	900	El?as Wismayer Rolando	mexrp16s	pvem
#531	401	Guerrero Rubio Pilar	mexrp17p	pvem
#532	901	Portilla Di?guez Manuel	mexrp17s	pvem
#533	402	Velasco Rodr?guez Ver?nica	mexrp18p	pvem
#534	902	Ram?rez Zollino Mar?a Fernanda	mexrp18s	pvem
#535	403	Garay Ulloa Silvano	mexrp19p	pt
#536	903	Gama Reynoso Ra?l	mexrp19s	pt
#537	404	Vela Gonz?lez Joaqu?n Humberto	mexrp20p	pt
#538	904	S?nchez Estrada Ma. del Rosario	mexrp20s	pt
#539	405	Arriola Gordillo M?nica	mexrp21p	panal
#540	905	Garc?a Guerrero Luis Alberto	mexrp21s	panal
#541	406	Castillo N?jera Ariel	mexrp22p	panal
#542	906	Romero V?zquez Gabriel	mexrp22s	panal
#543	407	Conde Rodr?guez Elsa De Guadalupe	mexrp23p	asd
#544	907	Rodr?guez L?pez Ver?nica	mexrp23s	asd
#545	408	Barr?n Mart?nez Claudia Isabel	mexrp24p	asd
#546	908	Garc?a M?ndez Armando	mexrp24s	asd
#547	166	Soto S?nchez Antonio	mic01p	prd
#548	666	Infante Gonz?lez Ricardo	mic01s	prd
#549	167	Villica?a Garc?a Rafael	mic02p	prd
#550	667	Cachu Aguilar Jos?	mic02s	prd
#551	168	Vallejo Est?vez Mario	mic03p	prd
#552	668	Rodr?guez Contreras Sa?l	mic03s	prd
#553	169	D?az Garibay Felipe	mic04p	pan
#554	669	Ayala Mart?nez Sonia	mic04s	pan
#555	170	Ceja Romero Ram?n	mic05p	pan
#556	670	Aguilar Solorio Rosa Mar?a	mic05s	pan
#557	171	R?os Gamboa Ra?l	mic06p	prd
#558	671	Bucio G?mez Alejandro	mic06s	prd
#559	172	Alonso Razo Humberto Wilfrido	mic07p	prd
#560	672	Maga?a Mendoza Arturo	mic07s	prd
#561	173	Ch?vez Garc?a Daniel	mic08p	pan
#562	673	Carrasco C?rdenas Susana Sarah?	mic08s	pan
#563	174	Mendoza Maldonado Fausto Fluvio	mic09p	prd
#564	674	Estrada Mendoza Conrrado	mic09s	prd
#565	175	Espinosa Pi?a Jos? Luis	mic10p	pan
#566	675	Gonz?lez Mart?nez Laura	mic10s	pan
#567	176	M?rquez Tinoco Francisco	mic11p	prd
#568	676	Escuadra G?mez Manuel	mic11s	prd
#569	177	Mendoza Mendoza Irineo	mic12p	prd
#570	677	Vallejo Vega V?ctor Hugo	mic12s	prd
#571	409	?lvarez Bernal Mar?a Elena	micrp01p	pan
#572	909	Loyola Trujillo Jos?	micrp01s	pan
#573	410	Berber Mart?nez Antonio	micrp02p	pan
#574	910	Saucedo Ch?vez Elia	micrp02s	pan
#575	411	Tamayo Herrera Yadhira Yvette	micrp03p	pan
#576	911	Paz Garibay Jos? Mar?a	micrp03s	pan
#577	412	Arellano Pulido Miguel ?ngel	micrp04p	prd
#578	912	Calder?n Torreblanca Fidel	micrp04s	prd
#579	413	L?pez Barriga Erick	micrp05p	prd
#580	913	Escobar Garc?a Her?n Agust?n	micrp05s	prd
#581	414	Ojeda Hern?ndez Concepci?n	micrp06p	prd
#582	914	Lazo De La Vega De Castro Cecilia	micrp06s	prd
#583	415	Soriano S?nchez Rosa Elva	micrp07p	prd
#584	915	T?llez Garc?a Clara	micrp07s	prd
#585	416	Orihuela B?rcenas Jos?	micrp08p	pri
#586	916	Arias Flores Marisol del Socorro	micrp08s	pri
#587	417	Reyna Garc?a Jos? Jes?s	micrp09p	pri
#588	917	Cerrillo Garnica Jos? Luis	micrp09s	pri
#589	418	Velasco P?rez Juan Carlos	micrp10p	pri
#590	918	Serrano Salazar Oziel	micrp10s	pri
#591	178	Iragorri Dur?n Enrique	mor01p	pan
#592	678	Utrilla Nieto Olivia Ver?nica	mor01s	pan
#593	179	Rom?n Isidoro Demetrio	mor02p	pan
#594	679	Borunda Mu?oz Luc?a Marcia	mor02s	pan
#595	180	Franco Melgarejo Rafael	mor03p	prd
#596	680	Cort?s Garc?a Ariel	mor03s	prd
#597	181	Orihuela Trejo Jos? Amado	mor04p	pri
#598	681	Rodr?guez Mart?nez Alicia	mor04s	pri
#599	182	S?nchez Trujillo Jos? V?ctor	mor05p	pan
#600	682	Rivera Castillo Victoria	mor05s	pan
#601	419	Bola?os Aguilar Edmundo Javier	morrp01p	pan
#602	919	G?mez Flores Consuelo	morrp01s	pan
#603	420	Vieyra Olivares Adriana Rebeca	morrp02p	pan
#604	920	Mar?n M?ndez Martin	morrp02s	pan
#605	421	D?az Contreras Adriana	morrp03p	prd
#606	921	Vergara Berm?dez Mar?a Guadalupe	morrp03s	prd
#607	422	Palma Cesar V?ctor Samuel	morrp04p	pri
#608	922	Mart?nez Chavarr?a Galindo Sergio	morrp04s	pri
#609	423	Estrada Gonz?lez Faustino Javier	morrp05p	pvem
#610	923	Bravo Mart?nez Esveida	morrp05s	pvem
#611	183	Gonz?lez Garc?a Sergio	nay01p	pri
#612	683	Partida Guzm?n Martha Roc?o	nay01s	pri
#613	184	Jim?nez Valenzuela Mar?a Eugenia	nay02p	prd
#614	684	L?pez Jim?nez Jorge	nay02s	prd
#615	185	Sandoval Paredes Sergio	nay03p	pri
#616	685	Ocampo Santana Karen	nay03s	pri
#617	424	Medina Rodr?guez Delber	nayrp01p	pan
#618	924	Villafranca Aguirre Fernanda Elvira	nayrp01s	pan
#619	425	Ibarra Franquez Sonia Nohelia	nayrp02p	prd
#620	925	Ortiz Ram?rez Jorge Alejandro	nayrp02s	prd
#621	426	Navarro Quintero Miguel ?ngel	nayrp03p	prd
#622	926	Porras Dom?nguez Mar?a Dolores	nayrp03s	prd
#623	427	R?os Camarena Alfredo Adolfo	nayrp04p	pri
#624	927	Morales Aceves Francisco Javier	nayrp04s	pri
#625	428	Castillo Romero Patricia Obdulia De Jes?s	nayrp05p	conve
#626	928	Fox Pe?a ?lvaro Augusto	nayrp05s	conve
#627	429	Cervantes Rivera Jaime	nayrp06p	pt
#628	929	Reynoso Esparza Juli?n Ezequiel	nayrp06s	pt
#629	186	Garc?a M?ller Martha Margarita	nl01p	pan
#630	686	Valdez L?pez Daniel	nl01s	pan
#631	187	Medina De La Cruz Rodrigo	nl02p	pri
#632	687	De La Garza Trevi?o Jorge Luis	nl02s	pri
#633	188	Murillo Torres Jos? Luis	nl03p	pan
#634	688	Cant? Moreno Marcela	nl03s	pan
#635	189	Ram?rez Villarreal Gustavo	nl04p	pan
#636	689	Garc?a Hern?ndez Gloria Socorro	nl04s	pan
#637	190	Caballero Camargo Gustavo Fernando	nl05p	pri
#638	690	Llarena Menard Carla Paola	nl05s	pri
#639	191	Barrios Rodr?guez Juan Enrique	nl06p	pan
#640	691	Gonz?lez Rivera Gloria Leticia	nl06s	pan
#641	192	Casta?o Contreras Cristi?n	nl07p	pan
#642	692	G?mez Garza Diana Esperanza	nl07s	pan
#643	193	Zambrano Elizondo Javier Mart?n	nl08p	pan
#644	693	Garza Guerra Carolina Mar?a	nl08s	pan
#645	194	Salas L?pez Ram?n	nl09p	pri
#646	694	T?llez Guzm?n Joel	nl09s	pri
#647	195	Villanueva Arjona Juan Manuel	nl10p	pan
#648	695	Campos Barroso Isabel Bibiana	nl10s	pan
#649	196	Rivera Bedoya Juan Francisco	nl11p	pri
#650	696	Alan?s De La Fuente Genaro	nl11s	pri
#651	197	Par?s Gonz?lez Juan Manuel	nl12p	pvem
#652	697	De Los Santos Gonz?lez Martha	nl12s	pvem
#653	430	L?pez Cisneros Jos? Mart?n	nlrp01p	pan
#654	930	Pe?a Dorado Mar?a del Carmen	nlrp01s	pan
#655	431	Orozco Ruiz Velazco Marco Heriberto	nlrp02p	pan
#656	931	Caballero Ch?vez Claudia Gabriela	nlrp02s	pan
#657	432	Canavati Tafich Jes?s Ricardo	nlrp03p	pri
#658	932	Gonz?lez Salinas Placido	nlrp03s	pri
#659	433	Ram?rez Cerda Ana Mar?a	nlrp04p	pvem
#660	933	Gonz?lez Mac?as Jes?s	nlrp04s	pvem
#661	434	Cant? Garza Ricardo	nlrp05p	pt
#662	934	Campos Mireles Teodoro	nlrp05s	pt
#663	198	Dehesa Mora Daniel	oax01p	prd
#664	698	G?mez Rivera Te?dulo	oax01s	prd
#665	199	Villanueva Abraj?n Patricia	oax02p	pri
#666	699	Virgen Carrera V?ctor Manuel	oax02s	pri
#667	200	Hern?ndez Gayt?n Daisy Selene	oax03p	prd
#668	700	Zanabria Garc?a Elsa Asunci?n	oax03s	prd
#669	201	Mart?nez Mart?nez Carlos Roberto	oax04p	prd
#670	701	Mendoza V?squez Ra?l	oax04s	prd
#671	202	Altamirano Toledo Carlos	oax05p	prd
#672	702	Le?n Arag?n Alejandro	oax05s	prd
#673	203	Romero Guzm?n Rosa Elia	oax06p	pt
#674	703	Romero Solano Mat?as	oax06s	pt
#675	204	Toledo Luis Jorge	oax07p	pri
#676	704	Gurri?n Mat?as Daniel	oax07s	pri
#677	205	Varela Lagunas Jos? Luis	oax08p	conve
#678	705	Guzm?n Rodr?guez Pedro Celestino	oax08s	conve
#679	206	Cuevas C?rdova Oth?n	oax09p	prd
#680	706	Vargas Mart?nez C?sar Domingo	oax09s	prd
#681	207	Hern?ndez Silva Benjam?n	oax10p	prd
#682	707	Valencia Ram?rez Aida Fabiola	oax10s	prd
#683	208	De los Santos Molina Joaqu?n Conrado	oax11p	prd
#684	708	Merino Garc?a Marcela	oax11s	prd
#685	435	Carrasco Altamirano Di?doro	oaxrp01p	pan
#686	935	Z??iga D?az Patricia Guadalupe	oaxrp01s	pan
#687	436	Murat Casab Jos?	oaxrp02p	pri
#688	936	Matus Mart?nez H?ctor	oaxrp02s	pri
#689	437	Ordaz Jim?nez Ismael	oaxrp03p	pri
#690	937	D?az Hartz Pedro Alejandro	oaxrp03s	pri
#691	438	Melo Vel?zquez Jos? Francisco	oaxrp04p	conve
#692	938	Palma Olvera Carmen Patricia	oaxrp04s	conve
#693	439	L?pez Adame Antonio Xavier	oaxrp05p	pvem
#694	939	D?az Ordaz Casta??n Gerardo	oaxrp05s	pvem
#695	440	Maciel Ortiz Ma. Mercedes	oaxrp06p	pt
#696	940	Del Castillo Aguilar Mar?a del Rosario	oaxrp06s	pt
#697	441	Pi?eyro Arias Irma	oaxrp07p	panal
#698	941	Rosado Correa Cinthia Mabel	oaxrp07s	panal
#699	442	L?pez Lena Cruz Humberto	oaxrp08p	indep
#700	942	L?pez Lena Pineda Carlos Humberto	oaxrp08s	indep
#701	209	Amador Leal Alberto	pue01p	pri
#702	709	Esquitin Lastiri Rosa Mar?a	pue01s	pri
#703	210	Jim?nez Ramos Mar?a Esther	pue02p	pan
#704	710	Gorozpe Trevi?o Erik Osvaldo	pue02s	pan
#705	211	Fuentes Ortiz Jos? Guillermo	pue03p	pan
#706	711	Pati?o Espino Jos? De Jes?s H?ctor	pue03s	pan
#707	212	Herrera Coyac Wenceslao	pue04p	pri
#708	712	Gonz?lez Ramos Reynaldo	pue04s	pri
#709	213	M?ndez Meneses Apolonio	pue05p	pan
#710	713	Mar?n Cort?s Sof?a Leticia	pue05s	pan
#711	214	Flores Grande Arturo	pue06p	pan
#712	714	Montero Machorro Mar?a Josefina	pue06s	pan
#713	215	Contreras Coeto Jos? Luis	pue07p	pan
#714	715	Salinas Hern?ndez Hilda Leticia	pue07s	pan
#715	216	Vasconcelos Rueda Antonio	pue08p	pan
#716	716	Montiel Contreras Jos? Benito Amado	pue08s	pan
#717	217	Lagunes Viveros Violeta del Pilar	pue09p	pan
#718	717	G?mez Barrales Ernesto Javier	pue09s	pan
#719	218	Parra Jim?nez Dolores Mar?a del Carmen	pue10p	pan
#720	718	Xicotencatl Hern?ndez Jos? Toribio Miguel	pue10s	pan
#721	219	Bello P?rez Alfonso Oth?n	pue11p	pan
#722	719	Rub?n D?vila Martha Ang?lica	pue11s	pan
#723	220	S?nchez D?az De Rivera Antonio	pue12p	pan
#724	720	Perroni Merino Gloria Mar?a	pue12s	pan
#725	221	Vel?zquez Guti?rrez Jos? Guillermo	pue13p	pan
#726	721	Garc?a Esparza Dolores	pue13s	pan
#727	222	Estefan Chidiac Charbel Jorge	pue14p	pri
#728	722	Herrera Mentado Raymundo	pue14s	pri
#729	223	Lezama Aradillas Ren?	pue15p	pan
#730	723	Ortiz Montoro Sagrario Mar?a del Rosario	pue15s	pan
#731	224	Mendoza Cort?s Mario	pue16p	pri
#732	724	L?pez Balbuena Guillermina	pue16s	pri
#733	443	D?az Garc?a Jos? Antonio	puerp01p	pan
#734	943	Lezama ?lvarez Tirsa Teresa	puerp01s	pan
#735	444	Fraile Garc?a Francisco Antonio	puerp02p	pan
#736	944	Guevara Uribe Abel	puerp02s	pan
#737	225	Garz?n Contreras Neftal?	puerp03p	prd
#738	725	M?ndez Spinola Jorge	puerp03s	prd
#739	226	Vel?zquez Aguirre Jes?s Evodio	puerp04p	prd
#740	726	Morales Manzo Jes?s Ricardo	puerp04s	prd
#741	227	Rubio Ch?vez Jos? Ignacio Alberto	que01p	pan
#742	727	Rubio M?ndez Ma. Micaela	que01s	pan
#743	228	Dom?nguez Servi?n Francisco	que02p	pan
#744	728	Rodr?guez Montes Bibiana	que02s	pan
#745	445	Jim?nez del Castillo Ma. de los ?ngeles	que03p	pan
#746	945	S?nchez Dur?n Herbert	que03s	pan
#747	446	Delgado Oscoy Alejandro Enrique	que04p	pan
#748	946	Gonz?lez Ram?rez Ma. Guadalupe	que04s	pan
#749	447	Arredondo Vel?zquez Jes?s	querp01p	pan
#750	947	?lvarez Soto Andrea	querp01s	pan
#751	448	V?zquez Mart?nez Alberto	querp02p	pan
#752	948	Cort?s Osornio Luz Virginia	querp02s	pan
#753	449	Ortiz Proal Mauricio	querp03p	pri
#754	949	Alcocer Gamba Liliana	querp03s	pri
#755	450	Aguilera Rico Jos? Luis	querp04p	conve
#756	950	P?rez Almanza Vicente	querp04s	conve
#757	451	Castellanos Cort?s Sara Isabel	querp05p	pvem
#758	951	Sesma Su?rez Jes?s	querp05s	pvem
#759	229	Ruiz Ch?vez Sara Latife	qui01p	pri
#760	729	Gonz?lez Hern?ndez Juan Carlos	qui01s	pri
#761	230	Espinosa Abuxapqui Eduardo El?as	qui02p	pri
#762	730	Pech Santos Julio Alberto	qui02s	pri
#763	231	Garmendia Hern?ndez Yolanda Mercedes	qui03p	pan
#764	731	Arellano S?nchez Sergio Alejandro	qui03s	pan
#765	452	Joaqu?n Coldwell Addy Cecilia	quirp01p	pan
#766	952	Rubio Piedra Ramiro	quirp01s	pan
#767	232	Medell?n Varela Antonio	san01p	pan
#768	732	Quistian Rangel Roc?o Alba	san01s	pan
#769	233	Leura Gonz?lez Agust?n	san02p	pan
#770	733	Camacho Mercado Aurora	san02s	pan
#771	234	Rodr?guez Uresti Enrique	san03p	pan
#772	734	Mart?nez P?rez Alicia	san03s	pan
#773	235	Lara Compe?n David	san04p	pan
#774	735	Loo Lizcano Regina Aurora	san04s	pan
#775	236	Garc?a Reyes Beatriz Eugenia	san05p	pan
#776	736	Garc?a Briones Marco Antonio	san05s	pan
#777	237	Degante Romero Silvia Emilia	san06p	pan
#778	737	Arellano Vera Gerardo	san06s	pan
#779	238	Rivera Rivera Jos? Guadalupe	san07p	pan
#780	738	Mart?nez Garc?a Nohemi	san07s	pan
#781	453	Carbajal M?ndez Liliana	sanrp01p	pan
#782	953	Hern?ndez ?vila Marco Antonio	sanrp01s	pan
#783	454	D?az De Le?n Torres Leticia	sanrp02p	pan
#784	954	Romero Baldazo Jos? Luis	sanrp02s	pan
#785	455	Ram?rez Stabros Jes?s	sanrp03p	pri
#786	955	Na?ez Rodr?guez Ang?lica	sanrp03s	pri
#787	239	Pe?uelas Acu?a Mayra Gisela	sin01p	pri
#788	739	Ram?rez Ruiz Carlos	sin01s	pri
#789	240	Vargas Landeros Gerardo Octavio	sin02p	pri
#790	740	Zamora Lugo Dolores	sin02s	pri
#791	241	Ojeda Camacho Gilberto	sin03p	pri
#792	741	Godoy Gaxiola Melchor	sin03s	pri
#793	242	Barajas L?pez Ram?n	sin04p	pri
#794	742	Armenta Ordu?o Trinidad	sin04s	pri
#795	243	Ortiz Hern?ndez Eduardo	sin05p	pan
#796	743	Flores Ch?vez Carlos Rafael	sin05s	pan
#797	244	Amador Gaxiola Daniel	sin06p	pri
#798	744	Arias Rodr?guez Mar?a del Carmen	sin06s	pri
#799	245	Patr?n Montalvo Jes?s Manuel	sin07p	pri
#800	745	Beltr?n Valenzuela Jes?s Martina	sin07s	pri
#801	246	Felton Gonz?lez Carlos Eduardo	sin08p	pan
#802	746	Vega Olivas Nadia Haydee	sin08s	pan
#803	456	Alcaraz Hern?ndez Alma Edwviges	sinrp01p	pan
#804	956	Ahumada Inzunza V?ctor Manuel	sinrp01s	pan
#805	457	Valenzuela Garc?a Mar?a Gloria Guadalupe	sinrp02p	pan
#806	957	Valenzuela L?pez Alejo	sinrp02s	pan
#807	458	Aguilar Acu?a Diego	sinrp03p	pri
#808	958	Sandoval D?az Jorge Arist?teles	sinrp03s	pri
#809	459	C?rdenas Fonseca Manuel	sinrp04p	panal
#810	959	Talamante Lemas Dora Mar?a Guadalupe	sinrp04s	panal
#811	247	Palafox N??ez Jos? In?s	son01p	pan
#812	747	Ortiz Pino Francisca	son01s	pan
#813	248	Navarro Sugich Carlos Alberto	son02p	pan
#814	748	C?rdova De La Cruz Luz Esthela	son02s	pan
#815	249	Serrato Castell Luis Gerardo	son03p	pan
#816	749	Coronel Gandara Mar?a Dolores	son03s	pan
#817	250	Zatarain Gonz?lez Carlos Ernesto	son04p	pri
#818	750	S?nchez Pe?uelas Salvador	son04s	pri
#819	251	Rodr?guez Ahumada Luis Fernando	son05p	pan
#820	751	Molina Freaner Rebeca Josefina	son05s	pan
#821	252	F?lix Holgu?n Armando Jes?s	son06p	pan
#822	752	Flores Moreno Lizbeth	son06s	pan
#823	253	Mendivil Ampar?n Gustavo	son07p	pri
#824	753	Verdugo Rosas Juan Manuel	son07s	pri
#825	460	Aranda Orozco Gerardo	sonrp01p	pan
#826	960	M?ndez Vargas Claudia	sonrp01s	pan
#827	461	Corral Aguilar Mar?a Mercedes	sonrp02p	pan
#828	961	Palafox Olivarria Cipriano	sonrp02s	pan
#829	462	Larios C?rdova H?ctor	sonrp03p	pan
#830	962	L?pez Noriega Alejandra	sonrp03s	pan
#831	463	Figueroa Ortega David	sonrp04p	pan
#832	963	Ram?rez Corral Ivette Jacqueline	sonrp04s	pan
#833	464	Navarro L?pez Carlos Ernesto	sonrp05p	prd
#834	964	Aguilar Reinal Luis Pavel	sonrp05s	prd
#835	465	Biebrich Torres Carlos Armando	sonrp06p	pri
#836	965	Oceguera Sandoval Elvy Ross	sonrp06s	pri
#837	254	S?nchez Cabrales Rafael El?as	tab01p	prd
#838	754	Pinz?n Herrera Jos? Alberto	tab01s	prd
#839	255	S?nchez Ramos Francisco	tab02p	prd
#840	755	Arjona Torres Silvia Elena	tab02s	prd
#841	256	Dagdug Lutzow Mois?s F?lix	tab03p	prd
#842	756	D?az V?zquez Gregorio	tab03s	prd
#843	257	Mayans Canabal Fernando Enrique	tab04p	prd
#844	757	P?rez V?zquez Jos? Domingo	tab04s	prd
#845	258	?lvarez Ram?n Silbestre	tab05p	prd
#846	758	Chi?as Ram?rez Carlos	tab05s	prd
#847	259	Fern?ndez Balboa M?nica	tab06p	prd
#848	759	Sansores Sastre Antonio	tab06s	prd
#849	466	Priego Tapia Gerardo	tabrp01p	pan
#850	966	Consospo Rodr?guez Bertha Araceli	tabrp01s	pan
#851	467	Rodr?guez Prats Juan Jos?	tabrp02p	pan
#852	967	Ferrer Rodr?guez Mar?a Esther	tabrp02s	pan
#853	468	Landero L?pez Pedro	tabrp03p	prd
#854	968	De La Cruz Romero Arnulfo	tabrp03s	prd
#855	469	Mendoza Flores Roberto	tabrp04p	prd
#856	969	Ferrer Abalos Oscar	tabrp04s	prd
#857	470	Izquierdo Bustamante Alfonso Rolando	tabrp05p	pri
#858	970	Vallejo Utrilla Aischa	tabrp05s	pri
#859	471	Garc?a Noriega Mar?a Guadalupe Josefina	tabrp06p	pvem
#860	971	Bellizzia Rosique Pascual	tabrp06s	pvem
#861	260	Garza Garza Horacio Emigdio	tam01p	pri
#862	760	P?rez Moreno Elodia	tam01s	pri
#863	261	Garc?a Vivi?n Ra?l	tam02p	pan
#864	761	S?enz Sustaita Ma. del Consuelo	tam02s	pan
#865	262	L?pez Reyna Omeheira	tam03p	pan
#866	762	Reyes Silva Jorge Manuel	tam03s	pan
#867	263	Garc?a Gonz?lez Carlos Alberto	tam04p	pan
#868	763	Rodr?guez Estrada Sandra Mar?a	tam04s	pan
#869	264	Gonz?lez Salum Miguel ?ngel	tam05p	pri
#870	764	S?nchez ?lvarez Carmen De Guadalupe	tam05s	pri
#871	265	C?rdenas del Avellano Enrique	tam06p	pri
#872	765	T?llez Delgadillo Luis Fernando	tam06s	pri
#873	266	Collado Lara Beatriz	tam07p	pan
#874	766	Y??ez Hern?ndez Rub?n	tam07s	pan
#875	267	Mej?a Garc?a Luis Alonso	tam08p	pan
#876	767	Y??ez De La Garza Elvira Lily	tam08s	pan
#877	472	C?rdenas Guti?rrez Gustavo Adolfo	tamrp01p	pan
#878	972	Shej Guzm?n Sara	tamrp01s	pan
#879	473	Ver?stegui Ostos C?sar Augusto	tamrp02p	pan
#880	973	Gonz?lez L?pez Crystal Georgina	tamrp02s	pan
#881	474	Bernal Guti?rrez Andr?s Marco Antonio	tamrp03p	pri
#882	974	Monta?ez Rivera Felisa	tamrp03s	pri
#883	475	Gloria Requena Tom?s	tamrp04p	pri
#884	975	Ramos Franco Jos? De Jes?s	tamrp04s	pri
#885	268	Aguilar L?pez Jos? Alejandro	tla01p	pan
#886	768	Salazar Anaya Mar?a Guadalupe	tla01s	pan
#887	269	Escobar Jardinez Adolfo	tla02p	pan
#888	769	Malcos Amaro Mar?a Ofelia Gloria	tla02s	pan
#889	270	Amaro Corona Alberto	tla03p	prd
#890	770	Vela Trejo V?ctor	tla03s	prd
#891	476	D?vila Fern?ndez Adriana	tlarp01p	pan
#892	976	Bernal Le?n Jaime Raymundo	tlarp01s	pan
#893	477	Mart?nez Hern?ndez Alejandro	tlarp02p	prd
#894	977	Robles G?mez Manuel Alejandro	tlarp02s	prd
#895	478	Gonz?lez Zarur Mariano	tlarp03p	pri
#896	978	Gonz?lez y G?mez Mario Alberto	tlarp03s	pri
#897	271	Pulido Pecero Pedro	ver01p	pan
#898	771	Cancelado Cancelado Cancelado	ver01s	pan
#899	272	Pinete Vargas Mar?a del Carmen	ver02p	pri
#900	772	Santos Larios Andr?s	ver02s	pri
#901	273	Laviada Hern?ndez I?igo Antonio	ver03p	pan
#902	773	Estopier Gonz?lez Griselda	ver03s	pan
#903	274	Deschamps Falc?n ?ngel Rafael	ver04p	pan
#904	774	Morales Utrera Mercedes	ver04s	pan
#905	275	Del Valle Toca Antonio	ver05p	pan
#906	775	Castro Ortiz Rosa Elva	ver05s	pan
#907	276	Del R?o Virgen Jos? Manuel	ver06p	conve
#908	776	Garc?a Garc?a Norma Jovita	ver06s	conve
#909	277	De La Torre S?nchez Jos?	ver07p	pan
#910	777	Mart?nez D?az Mar?a De Jes?s	ver07s	pan
#911	278	Salas Contreras Marcos	ver08p	pan
#912	778	Apodaca Qui?ones Cirina	ver08s	pan
#913	279	Mota Hern?ndez Adolfo	ver09p	pri
#914	779	Arcos Suarez Armando	ver09s	pri
#915	280	Morales Garc?a Elizabeth	ver10p	pvem
#916	780	Carvajal Hern?ndez Roberto Carlos	ver10s	pvem
#917	281	Rasgado Corsi Gloria	ver11p	prd
#918	781	De La Cruz Silva Orlando	ver11s	prd
#919	282	Guti?rrez Lagunes Mar?a Victoria	ver12p	pan
#920	782	G?mez Valdivia Ricardo	ver12s	pan
#921	283	Mollinedo Hern?ndez Agust?n	ver13p	pan
#922	783	Carvajal Lagunes Gloria	ver13s	pan
#923	284	Uscanga Cruz Robinson	ver14p	conve
#924	784	Chaires Lugardo Agust?n	ver14s	conve
#925	285	Lagunes Gallina Gerardo	ver15p	pri
#926	785	Andrade Navarro Mar?a Ang?lica	ver15s	pri
#927	286	Duck N??ez Edgar Mauricio	ver16p	pan
#928	786	Hern?ndez Escobar Alma Rosa	ver16s	pan
#929	287	Castro De La Rosa Osiel	ver17p	pan
#930	787	Medina Tamayo Natividad	ver17s	pan
#931	288	Montalvo G?mez Pedro	ver18p	pri
#932	788	Ortega Tzitzihua Mar?a Dolores Luc?a	ver18s	pri
#933	289	Dom?nguez Dom?nguez Nemesio	ver19p	pri
#934	789	Quinto Hern?ndez Rafael	ver19s	pri
#935	290	Barradas Miravete Gregorio	ver20p	pan
#936	790	Casta?eda Arizmendi Sonia	ver20s	pan
#937	291	Lemarroy Mart?nez Juan Dar?o	ver21p	prd
#938	791	Dom?nguez L?pez Flavio	ver21s	prd
#939	479	Buganza Salmer?n Gerardo	verrp01p	pan
#940	979	Pineda Andrade Judith	verrp01s	pan
#941	480	Carrasco Altamirano Di?doro	verrp02p	pan
#942	980	Mora Cuevas Marisol	verrp02s	pan
#943	481	De la Torre Jaramillo Eduardo Sergio	verrp03p	pan
#944	981	Dom?nguez Cede?o Jorge Virgilio	verrp03s	pan
#945	482	Almaz?n Gonz?lez Jos? Antonio	verrp04p	prd
#946	982	Arredondo Mart?nez Oscar	verrp04s	prd
#947	483	Arag?n Castillo Irene	verrp05p	prd
#948	983	Mena Colli Porfiria	verrp05s	prd
#949	484	Condado Escamilla Cuitlahuac	verrp06p	prd
#950	984	Ortiz Garc?a Juan Manuel	verrp06s	prd
#951	485	Pulido Santiago Celso David	verrp07p	prd
#952	985	?vila Estrada Adri?n Sigfrido	verrp07s	prd
#953	586	Aldana Prieto Luis Ricardo	verrp08p	pri
#954	986	Sabines Chesterking Julio ?ngel	verrp08s	pri
#955	487	Badillo Mart?nez Roberto	verrp09p	pri
#956	987	Molina Gonz?lez Mar?a Xochitl	verrp09s	pri
#957	488	P?rez Vald?s Daniel	verrp10p	pri
#958	988	Morales Vel?zquez Mar?a Isabel	verrp10s	pri
#959	489	Salvatori Bronca Mar?a del Carmen	verrp11p	conve
#960	989	Vargas P?rez Nelly del Carmen	verrp11s	conve
#961	490	Cobo Terrazas Diego	verrp12p	pvem
#962	990	Padilla Acevedo Jorge Alberto	verrp12s	pvem
#963	491	Elizondo Garrido Francisco	verrp13p	pvem
#964	991	Ferrat Mancera Alain	verrp13s	pvem
#965	292	D?az Mena Joaqu?n Jes?s	yuc01p	pan
#966	792	Alcocer y Gazca Teresa De Jes?s	yuc01s	pan
#967	293	Blanco Paj?n Jos? Luis	yuc02p	pri
#968	793	N??ez Zapata Mar?a Eugenia del Pilar	yuc02s	pri
#969	294	Castro Romero Mar?a Sof?a del Perpetuo Socorro	yuc03p	pan
#970	794	Antu?a Batista Fidel	yuc03s	pan
#971	295	Ram?rez Pech Edgar Mart?n	yuc04p	pan
#972	795	Rodr?guez Sabido Dolores del Socorro	yuc04s	pan
#973	296	Escaroz Soler Gerardo Antonio	yuc05p	pan
#974	796	Azarcoya Guti?rrez Gaspar Manuel	yuc05s	pan
#975	492	Medina Rodr?guez Lizbeth	yucrp01p	pan
#976	992	Novelo Ku Francisco Javier	yucrp01s	pan
#977	493	Gamboa Patr?n Emilio	yucrp02p	pri
#978	993	Ruiz Ponce Madrid Esteban	yucrp02s	pri
#979	494	Escalante Jasso Araceli	yucrp03p	pri
#980	994	Peraza Valdez Ismael	yucrp03s	pri
#981	495	Rojas Guti?rrez Carlos	yucrp04p	pri
#982	995	Muza Sim?n Sara Esther	yucrp04s	pri
#983	297	Monreal ?vila Susana	zac01p	prd
#984	797	Norman Giacoman Julio Ysaac	zac01s	prd
#985	298	Berm?dez Viramontes Andr?s	zac02p	pan
#986	798	Bernal Frausto Federico	zac02s	pan
#987	299	C?rdenas Hern?ndez Raymundo	zac03p	prd
#988	799	G?mez Ure?o Ismael	zac03s	prd
#989	300	Calzada V?zquez Francisco Javier	zac04p	prd
#990	800	Garc?a Saucedo Javier	zac04s	prd
#991	496	Borrego Estrada Felipe	zacrp01p	pan
#992	996	Arg?elles Arellano Mar?a del Consuelo	zacrp01s	pan
#993	497	Cervantes Rodr?guez Aurora	zacrp02p	prd
#994	997	Chaidez Castillo Santa Blanca	zacrp02s	prd
#995	498	M?rquez Madrid Camerino Eleazar	zacrp03p	prd
#996	998	Mart?nez L?pez Luis	zacrp03s	prd
#997	499	Puente Salas Carlos Alberto	zacrp04p	pvem
#998	999	Vel?zquez Beeck Ana Teresa	zacrp04s	pvem
#999	500	Gonz?lez S?nchez Dolores	zacrp05p	pan
#1000	1000	Gallegos Soto Benjam?n	zacrp05s	pan

coord <- c("", "", "", "", "", "", "", "", "", "", "", "", "",
"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
"", "", "", "", "", "", "", "", "", "", "", "Glez Garza", "",
"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
"", "", "", "", "", "", "", "", "", "", "", "", "", "",
"Iturbe", "", "", "", "", "", "Lavara", "", "", "", "", "", "",
"", "", "", "Arvizu", "", "", "", "", "", "", "", "", "", "",
"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
"", "Cant?", "", "", "", "", "", "", "", "", "", "", "", "",
"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
"", "", "", "", "", "", "", "", "", "", "", "Larios", "", "",
"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
"", "Gamboa", "", "", "", "", "", "", "", "", "", "", "", "",
"", "", "", "", "", "", "", "", "", "", "")
##
dcoord <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 1, NA, NA, NA, NA,
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 1, NA, NA, NA, NA,
NA, 1, NA, NA, NA, NA, NA, NA, NA, NA, NA, 1, NA, NA, NA, NA,
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
NA, NA, NA, NA, NA, NA, NA, 1, NA, NA, NA, NA, NA, NA, NA, NA,
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 1,
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
NA, NA, NA, 1, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
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
I <- dim(rc)[1]
for (i in 1:I){
    votdat$ayes[i] <- count.votes(rc[i,])[1]
    votdat$nays[i] <- count.votes(rc[i,])[2]
    votdat$valid[i] <- count.votes(rc[i,])[3]
    votdat$abs[i] <- count.votes(rc[i,])[4]
    votdat$tot[i] <- count.votes(rc[i,])[5]
              }
##
## DEPUTY'S ABSTENTION RATE
noVoteRate <- as.numeric(rc[1,])
for (d in 1:dim(rc)[2]){
    noVoteRate[d] <- count.votes(rc[,d])[4] / count.votes(rc[,d])[5]
#    noVoteRate[d] <- length(rc[rc[,d]==0,d])/dim(rc)[1]
    }
dipdat$noVoteRate <- noVoteRate
##
## DROP DIPUTADOS WHO NEVER PLEDGED
dropUnpledged <- rep(0, times=1000)
for (d in 1:1000){
    dropUnpledged[d] <- ifelse(count.votes(rc[,d])[3]==0, -1, 0)
                 }
##
length(dropUnpledged[dropUnpledged<0])  ## HOW MANY NEVER PLEDGED (DESCRIPTIVES)
##
dropUnpledged <- dropUnpledged * (1:1000)
dropUnpledged <- dropUnpledged[dropUnpledged<0] ## NEGATIVE INDEX FOR THOSE WHO NEVER PLEDGED
rc <- rc[,dropUnpledged]; dipdat <- dipdat[dropUnpledged,]
tmp <- rc; tmp <- t(tmp); table(tmp) ## CHECA QUE TODO SEA -1 0 1
rm(d, tmp, dropUnpledged, noVoteRate)
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
rm(d, D, dropAbstainers)
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
rm(I, i, dropUncontested)
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
rm(dropUninformative, I)




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




#########################################################################
###   Static 2Dimensions three item anchors, irt paremeterization     ###
#########################################################################

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

###########
## MODEL ##
###########
# # runjags version
# modelSta2Di.irt = "model {
# rjags version
cat("
model {
for (j in 1:J){                ## loop over diputados
    for (i in 1:I){              ## loop over items
#old#      #v.hat[j,i] ~ dbern(p[j,i]);                            ## voting rule
#old#      #p[j,i] <- phi(v.star[j,i]);                            ## sets 0<p<1
#old#      v.star[j,i] ~ dnorm(mu[j,i],1)T(lo.v[j,i],hi.v[j,i]);   ## truncated normal sampling
      v[j,i] ~ dbern(p[j,i]);                                 ## voting rule
      probit(p[j,i]) <- mu[j,i];                              ## sets 0<p<1 as function of mu
      mu[j,i] <- beta[i]*x[j] - alpha[i] + delta[i]*y[j];     ## utility differential
                  }
                }
## ESTO LO PUEDO SACAR POST ESTIMACION
##  for (i in 1:I){
##  a[i] <- beta[i] / delta[i]  ## pendiente de cutline
##  b[i] <- alpha[i] / delta[i] ## constante de cutline
##  }
  ## priors ################
for (j in 1:J){
    x[j] ~  dnorm(0, 1)
    y[j] ~  dnorm(0, 1)
    }
for(i in 1:(west-1)){
    alpha[i] ~ dnorm( 0, 1)
    beta[i]  ~ dnorm( 0, 1)
    delta[i] ~ dnorm( 0, 1)
    }
alpha[west] ~ dnorm( 0, 1)
beta[west]  ~ dnorm(-4, 20)  ## INFORMATIVE
delta[west] ~ dnorm(-4, 20)  ## INFORMATIVE
for(i in (west+1):(east-1)){
    alpha[i] ~ dnorm( 0, 1)
    beta[i]  ~ dnorm( 0, 1)
    delta[i] ~ dnorm( 0, 1)
    }
alpha[east] ~ dnorm( 0, 1)
beta[east]  ~ dnorm( 4, 20) ## INFORMATIVE
delta[east] ~ dnorm( 4, 20) ## INFORMATIVE
for(i in (east+1):(north-1)){
    alpha[i] ~ dnorm( 0, 1)
    beta[i]  ~ dnorm( 0, 1)
    delta[i] ~ dnorm( 0, 1)
    }
alpha[north] ~ dnorm( 0, 1)
beta[north]  ~ dnorm(-4, 20) ## INFORMATIVE
delta[north] ~ dnorm( 4, 20) ## INFORMATIVE
for(i in (north+1):I){
    alpha[i] ~ dnorm( 0, 1)
    beta[i]  ~ dnorm( 0, 1)
    delta[i] ~ dnorm( 0, 1)
    }
}"
, file="modelSta2Di.irt.jag")
#end model##############

J <- ncol(rc); I <- nrow(rc)
#old#v <- t(rc)
#old#lo.v <- ifelse(is.na(v)==TRUE | v==  1, 0, -5)
#old#hi.v <- ifelse(is.na(v)==TRUE | v== -1,  0, 5)
#old#vstar <- matrix (NA, nrow=J, ncol=I)
#old#for (j in 1:J){
#old#    for (i in 1:I){
#old#        vstar[j,i] <- ifelse(v[j,i]==0, 0, ifelse(v[j,i]==1, runif(1), -1*runif(1)))}}
#old#rm(v)
J <- ncol(rc); I <- nrow(rc)
v <- t(rc)
## RECODE v TO 0=nay NA=abstain 1=aye
v <- apply(v, 2, recode, recodes="-1=0; 0=NA")
# # runjags version
# dip.parameters <- c("delta","beta", "alpha", "x", "y") #, "deviance")
# dip.inits.1 <- dump.format (list(v.star=vstar, x = rnorm(J), y = rnorm(J), alpha = rnorm(I), delta = rnorm(I), beta = rnorm(I)))
# dip.inits.2 <- dump.format (list(v.star=vstar, x = rnorm(J), y = rnorm(J), alpha = rnorm(I), delta = rnorm(I), beta = rnorm(I)))
# dip.inits.3 <- dump.format (list(v.star=vstar, x = rnorm(J), y = rnorm(J), alpha = rnorm(I), delta = rnorm(I), beta = rnorm(I)))
# dip.data <- dump.format (list (J=J, I=I, lo.v=lo.v, hi.v=hi.v, west=west, east=east, north=north))
# rjags version
dip.parameters <- c("delta","beta", "alpha", "x", "y")
dip.inits<-function() {
  list(x = rnorm(J), y = rnorm(J), alpha = rnorm(I), delta = rnorm(I), beta = rnorm(I))
  }
dip.data <- list ("v"=as.matrix(v), "J"=J, "I"=I, "west"=west, "east"=east, "north"=north)

# ################################################################################
# snowJags <- function(   ## ADAPTED FROM olivella.gill.2011
#     post.name,     #Name for object that will contain posterior samples
#     model.name,    #Name of object containing JAGS model
#     monitors,      #Character vector with parameter names
#     n.chains=2,    #Number of parallel chains
#     data.list,     #List of data used in model
#     inits,         #Function giving initial values
#     thin,          #Value indicating thinning interval
#     burnin,        #Value indicating burn-in length
#     samples=2000,  #Samples taken from each chain for posterior
#     check=TRUE,    #Perform convergence diagnostics by default
#     plots=FALSE,   #
#    # where.jags,    #Where is jags installed (Windows requires, Unix maybe not)
#     nodes=2        #Number of cores
# )
# {
#     # CHECK FOR `snowfall', AND INSTALL IF NEEDED
#     hasSF <- require(snowfall)
#     if(hasSF){
#         library(snowfall)
#     }else{
#         cat("Package 'snowfall' required. Installing it now.\n")
#         install.packages("snowfall")
#         library(snowfall)
#     }
#     # CHECK FOR `rlecuyer', AND INSTALL IF NEEDED (FOR THE RNG)
#     hasLC <- require(rlecuyer)
#     if(hasLC){
#         library(rlecuyer)
#     }else{
#         cat("Package 'rlecuyer' required. Installing it now.\n")
#         install.packages("rlecuyer")
#         library(rlecuyer)
#     }
#     library(runjags)
#     # CREATE A FUNCTION THAT RUNS JAGS MODEL VIA runjags AND RETURNS A mcmc.list OBJECT
#     jags.runner <- function(counter){
#     postSAMPLES <- run.jags (
#          model=model.name,
#          monitor=monitors,
#          n.chains=1,
#          data=data.list,
#          inits=inits,
#          thin=,
#          burnin=burnin,
#          sample=samples,
#          check.conv=check,
#      #    jags=where.jags,
#          plots=plots
#          )
#     return(postSAMPLES)
#     }
#     # INITIALIZE A CLUSTER
#     sfInit(parallel=TRUE, cpus=nodes,type="SOCK",socketHosts=(rep("127.0.0.1",nodes)))
#     # EXPORT VARIABLES, runjags AND MCMCpack PACKAGES TO EVERY NODE
#     sfExportAll()
#     sfLibrary(runjags)
#     sfLibrary(MCMCpack)
#     # START NETWROK RN, TO AVOID PROBLEMS WITH USING A SINGLE COMPUTER'S RNG
#     sfClusterSetupRNG()
#     # SEND TASKS TO CLUSTERS USING LOAD BALANCING
#     result <- sfClusterApplyLB(1:n.chains, jags.runner)
#     # STOP CLUSTER!
#     sfStop()
#     # GATHER RESTULS AND RETURN mcmc.list OBJECT
#     chains <- result[[1]]
#     for(z in 2:n.chains){
#     chains[[z]]<-as.mcmc(result[[z]])
#     }
#     #post.name <- chains
#     return(chains)
# }
# #end snowJags##################################################################

#test ride to see program works
myWrap <- function(){
# runjags version
#    dip.60 <- run.jags (
#          model=modelSta2Di.irt,
#          monitor=dip.parameters,
#          n.chains=3,
#          data=dip.data,
#          inits=list( dip.inits.1, dip.inits.2, dip.inits.3 ),
#          thin=10,
#          burnin=5000,
#          sample=5000,
#          check.conv=FALSE,
#     #     jags = "c:/Archivos de programa/JAGS/JAGS-2.2.0/bin/jags-terminal.exe",
#     #     jags = "c:/Program Files (x86)/JAGS/JAGS-2.2.0/bin/jags-terminal.exe",
#     #     jags = "C:/Program Files/JAGS/JAGS-3.1.0/x64/bin/jags-terminal.exe",
#          plots=TRUE
#     )
    modelInit <- jags.model (
         "modelSta2Di.irt.jag",
         inits=dip.inits,
         data=dip.data,
         n.chains=1,
         n.adapt=9000)
    dip.60.3 <- coda.samples (modelInit,
         dip.parameters,
         n.iter=1000,
         thin=10)
#    return(dip.60)
}
print(system.time(myWrap()))

## COMBINE UNICHAINS RUN SEPARATELY: LOAD EACH CHAIN NAMED dip.60.ch, THEN:
load("tmp2.RData"); load("tmp3.RData")
dip.60 <- dip.60.1 ## so that it inherits mcmc.list attribute
dip.60[[2]] <- dip.60.2[[1]]
dip.60[[3]] <- dip.60.3[[1]]

## CONVERGENCE DIAGNOSTIC
gelman.diag(dip.60, confidence = 0.95, transform=FALSE, autoburnin=FALSE)

# #compare with parallel processing
# print(system.time(
#         snowJags(
#         post.name=dip.60,     #Name for object that will contain posterior samples
#         model.name=modelSta2Di.irt,    #Name of object containing JAGS model
#         monitors=dip.parameters,      #Character vector with parameter names
#         n.chains=3,    #Number of parallel chains
#         data.list=dip.data,     #List of data used in model
#         inits=list( dip.inits.1, dip.inits.2, dip.inits.3 ),         #Function giving initial values
#         thin=1,          #Value indicating thinning interval
#         burnin=5,        #Value indicating burn-in length
#         samples=5,  #Samples taken from each chain for posterior
#         check=FALSE,    #Perform convergence diagnostics by default
#         plots=FALSE,   #
#        # where.jags="C:/Program Files/JAGS/JAGS-3.1.0/x64/bin/jags-terminal.exe",    #Where is jags installed (Windows requires, Unix maybe not)
#         nodes=3        #Number of cores
# )
# ))

#to continue running in Bugs
tmp1<-list (
    v.star=vstar,
    delta=dip.60$last.values[[1]]$delta,
    angle=dip.60$last.values[[1]]$angle,
    b=dip.60$last.values[[1]]$b,
    x=dip.60$last.values[[1]]$x,
    y=dip.60$last.values[[1]]$y
    )
tmp2<-list (
    v.star=vstar,
    delta=dip.60$last.values[[2]]$delta,
    angle=dip.60$last.values[[2]]$angle,
    b=dip.60$last.values[[2]]$b,
    x=dip.60$last.values[[2]]$x,
    y=dip.60$last.values[[2]]$y
    )
tmp3<-list (
    v.star=vstar,
    delta=dip.60$last.values[[3]]$delta,
    angle=dip.60$last.values[[3]]$angle,
    b=dip.60$last.values[[3]]$b,
    x=dip.60$last.values[[3]]$x,
    y=dip.60$last.values[[3]]$y
    )
### for (chain in 1:3){dip.60$last.values[[chain]]$v.star <- vstar}
dip.60.2 <- bugs (dip.data,
                inits=list(tmp1,tmp2,tmp3),
                dip.parameters,
#                "modelSta2Dj.irt.txt", n.chains=3,
                "modelSta2Dj.txt", n.chains=3,
                n.iter=5000, n.thin=25, debug=T,
                bugs.directory = "c:/Program Files (x86)/WinBUGS14/",
#                bugs.directory = "c:/Archivos de programa/WinBUGS14/",
                program = c("WinBUGS"))

dip.60 <- dip.60.2
rm(dip.60.2)

#post.draw <- rbind(dip.60$mcmc[[1]], dip.60$mcmc[[2]], dip.60$mcmc[[3]])  ## runjags version
post.draw <- rbind(dip.60[[1]], dip.60[[2]], dip.60[[3]])  ## rjags version
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


dipdat$color[dipdat$color=="."] <- "black"
plot(c(-3,3), c(-3,3), type="n")
points(jotas[,2],jotas[,5], pch=19, col=dipdat$color)

tmp <- dimnames(dip.60[[1]]); tmp <- tmp[[2]]
cplt <- function(X)
    {
    tmp2 <- min(as.numeric(dip.60[[1]][,X]), as.numeric(dip.60[[2]][,X]), as.numeric(dip.60[[3]][,X]));
    tmp3 <- max(as.numeric(dip.60[[1]][,X]), as.numeric(dip.60[[2]][,X]), as.numeric(dip.60[[3]][,X]));
    plot(c(1,100), c(tmp2,tmp3), type="n", main=tmp[X]);
    lines(1:100, as.numeric(dip.60[[1]][,X]),col="red");
    lines(1:100, as.numeric(dip.60[[2]][,X]),col="blue");
    lines(1:100, as.numeric(dip.60[[3]][,X]),col="green");
    }
##
setwd(paste(workdir, "/graphs/convergence", sep=""))
for (i in 1:length(dip.60[[1]][1,]))
    {
    pdf( paste("tmp", i, ".pdf", sep=""))
    cplt(i)
    dev.off()
    }
setwd(workdir)

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






eric  ###################################################################
### static model in Two Dimensions, extremists -- IRT PARAMETERIZATION
###################################################################

## SORT BY PARTY
tmp <- 1:dim(rc)[1]
tmp <- ifelse ( dipdat$part=="pan", tmp,
        ifelse (dipdat$part=="pri", tmp+1000,
         ifelse ( dipdat$part=="prd", tmp+2000,
          ifelse ( dipdat$part=="pt", tmp+3000, tmp+4000))))
## rcold <- rc; dipold <- dipdat
rc <- rc[,order(tmp)]; dipdat <- dipdat[order(tmp),]
## PARTY INDICES (FOR ANCHORS)
PAN <- length(tmp[tmp<1001]); PRI <- PAN+length(tmp[tmp>1000&tmp<2001]);
PRD <- PRI+length(tmp[tmp>2000&tmp<3001]); PT <- PRD+length(tmp[tmp>3000&tmp<4001])

###########
## MODEL ##
###########
modelSta2Dj.irt = "model {
  for (j in 1:J){                ## loop over diputados
    for (i in 1:I){              ## loop over items
      #v.hat[j,i] ~ dbern(p[j,i]);                                  ## voting rule
      #p[j,i] <- phi(v.star[j,i]);                                  ## sets 0<p<1
      v.star[j,i] ~ dnorm(mu[j,i],1)T(lo.v[j,i],hi.v[j,i]);   ## truncated normal sampling
      mu[j,i] <- beta[i]*x[j] - alpha[i] + delta[i]*y[j] ## utility differential
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
    y[j] ~  dnorm(1, 4)
    }
for (j in (PAN+1):PRI){
    x[j] ~  dnorm(0, 4)    # PRI
    y[j] ~  dnorm(-1, 4)
    }
for (j in (PRI+1):PRD){
    x[j] ~  dnorm(-1, 4)    # PRD
    y[j] ~  dnorm(.5, 4)
    }
for (j in (PRD+1):PT){
    x[j] ~  dnorm(-1, 4)    # PT
    y[j] ~  dnorm(-1, 4)
    }
for (j in (PT+1):J){
    x[j] ~  dnorm(0, .1)
    y[j] ~  dnorm(0, .1)
    }
    for(i in 1:I){
        alpha[i] ~ dnorm( 0, 1)
        beta[i]  ~ dnorm( 0, 1)
        delta[i] ~ dnorm( 0, 1)
                 }
}
"
#end model##############

J <- ncol(rc); I <- nrow(rc)
v <- t(rc)
lo.v <- ifelse(is.na(v)==TRUE | v==  1, 0, -100)
hi.v <- ifelse(is.na(v)==TRUE | v== -1,  0, 100)
vstar <- matrix (NA, nrow=J, ncol=I)
for (j in 1:J){
    for (i in 1:I){
        vstar[j,i] <- ifelse(v[j,i]==0, 0, ifelse(v[j,i]==1, runif(1), -1*runif(1)))}}
rm(v)
dip.parameters <- c("delta","beta", "alpha", "x", "y") #, "deviance")
dip.inits.1 <- dump.format (list(v.star=vstar, x = rnorm(J), y = rnorm(J), alpha = rnorm(I), delta = rnorm(I), beta = rnorm(I)))
dip.inits.2 <- dump.format (list(v.star=vstar, x = rnorm(J), y = rnorm(J), alpha = rnorm(I), delta = rnorm(I), beta = rnorm(I)))
dip.inits.3 <- dump.format (list(v.star=vstar, x = rnorm(J), y = rnorm(J), alpha = rnorm(I), delta = rnorm(I), beta = rnorm(I)))
dip.data <- dump.format (list (J=J, I=I, lo.v=lo.v, hi.v=hi.v, PAN=PAN, PRI=PRI, PRD=PRD, PT=PT))

#test ride to see program works
#myWrap <- function(){
    dip.60 <- run.jags (
         model=modelSta2Dj.irt,
         monitor=dip.parameters,
         n.chains=3,
         data=dip.data,
         inits=list( dip.inits.1, dip.inits.2, dip.inits.3 ),
         thin=10,
         burnin=5000,
         sample=5000,
         check.conv=FALSE,
    #     jags = "c:/Archivos de programa/JAGS/JAGS-2.2.0/bin/jags-terminal.exe",
    #     jags = "c:/Program Files (x86)/JAGS/JAGS-2.2.0/bin/jags-terminal.exe",
    #     jags = "C:/Program Files/JAGS/JAGS-3.1.0/x64/bin/jags-terminal.exe",
         plots=FALSE
    )
#    return(dip.60)
#}
#print(system.time(myWrap()))

#to continue running in Bugs
tmp1<-list (
    v.star=vstar,
    delta=dip.60$last.values[[1]]$delta,
    angle=dip.60$last.values[[1]]$angle,
    b=dip.60$last.values[[1]]$b,
    x=dip.60$last.values[[1]]$x,
    y=dip.60$last.values[[1]]$y
    )
tmp2<-list (
    v.star=vstar,
    delta=dip.60$last.values[[2]]$delta,
    angle=dip.60$last.values[[2]]$angle,
    b=dip.60$last.values[[2]]$b,
    x=dip.60$last.values[[2]]$x,
    y=dip.60$last.values[[2]]$y
    )
tmp3<-list (
    v.star=vstar,
    delta=dip.60$last.values[[3]]$delta,
    angle=dip.60$last.values[[3]]$angle,
    b=dip.60$last.values[[3]]$b,
    x=dip.60$last.values[[3]]$x,
    y=dip.60$last.values[[3]]$y
    )
### for (chain in 1:3){dip.60$last.values[[chain]]$v.star <- vstar}
dip.60.2 <- bugs (dip.data,
                inits=list(tmp1,tmp2,tmp3),
                dip.parameters,
#                "modelSta2Dj.irt.txt", n.chains=3,
                "modelSta2Dj.txt", n.chains=3,
                n.iter=5000, n.thin=25, debug=T,
                bugs.directory = "c:/Program Files (x86)/WinBUGS14/",
#                bugs.directory = "c:/Archivos de programa/WinBUGS14/",
                program = c("WinBUGS"))

dip.60 <- dip.60.2
rm(dip.60.2)

post.draw <- rbind(dip.60$mcmc[[1]], dip.60$mcmc[[2]], dip.60$mcmc[[3]])
post.x <- post.draw[,grep("x", colnames(post.draw))]
post.y <- post.draw[,grep("y", colnames(post.draw))]

jotas <- matrix(NA, nrow=J, ncol=6)
for (j in 1:J){
    jotas[j,1] <- quantile (post.x[,j], 0.025, names=F)
    jotas[j,2] <- quantile (post.x[,j], 0.50, names=F)
    jotas[j,3] <- quantile (post.x[,j], 0.975, names=F)
    jotas[j,4] <- quantile (post.y[,j], 0.025, names=F)
    jotas[j,5] <- quantile (post.y[,j], 0.50, names=F)
    jotas[j,6] <- quantile (post.y[,j], 0.975, names=F)
    }


dipdat$color[dipdat$color=="."] <- "black"
plot(jotas[,2],jotas[,5], pch=19, col=dipdat$color)


eric  ###################################################################
### static model in Two Dimensions, extremists -- CUTLINE SPECIFICATION
###################################################################

## SORT BY PARTY
tmp <- 1:dim(rc)[1]
tmp <- ifelse ( dipdat$part=="pan", tmp,
        ifelse (dipdat$part=="pri", tmp+1000,
         ifelse ( dipdat$part=="prd", tmp+2000,
          ifelse ( dipdat$part=="pt", tmp+3000, tmp+4000))))
## rcold <- rc; dipold <- dipdat
rc <- rc[,order(tmp)]; dipdat <- dipdat[order(tmp),]
## PARTY INDICES (FOR ANCHORS)
PAN <- length(tmp[tmp<1001]); PRI <- PAN+length(tmp[tmp>1000&tmp<2001]);
PRD <- PRI+length(tmp[tmp>2000&tmp<3001]); PT <- PRD+length(tmp[tmp>3000&tmp<4001])

###########
## MODEL ##
###########
modelSta2Dj = "model {
  for (j in 1:J){                ## loop over diputados
    for (i in 1:I){              ## loop over items
      #y.hat[j,i] ~ dbern(p[j,i]);                                  ## voting rule
      #p[j,i] <- phi(v.star[j,i]);                                  ## sets 0<p<1
      v.star[j,i] ~ dnorm(mu[j,i],1)T(lo.v[j,i],hi.v[j,i]);   ## truncated normal sampling
      mu[j,i] <- delta[i]*a[i]*x[j] + delta[i]*b[i] - delta[i]*y[j] ## utility differential
                  }
                }
  for (i in 1:I){
  a[i] <- sin(angle[i]) / sqrt(1-sin(angle[i])*sin(angle[i]))
  }
  ## priors ################
for (j in 1:PAN){
    x[j] ~  dnorm(1, 4)   # PAN
    y[j] ~  dnorm(1, 4)
    }
for (j in (PAN+1):PRI){
    x[j] ~  dnorm(0, 4)    # PRI
    y[j] ~  dnorm(-1, 4)
    }
for (j in (PRI+1):PRD){
    x[j] ~  dnorm(-1, 4)    # PRD
    y[j] ~  dnorm(.5, 4)
    }
for (j in (PRD+1):PT){
    x[j] ~  dnorm(-1, 4)    # PT
    y[j] ~  dnorm(-1, 4)
    }
for (j in (PT+1):J){
    x[j] ~  dnorm(0, .1)
    y[j] ~  dnorm(0, .1)
    }
    for(i in 1:I){
        delta[i] ~ dnorm( 0, 0.1)
        angle[i] ~ dunif(-1.57,1.57) # (-pi/2,pi/2)
        b[i] ~ dnorm( 0, .1)
                 }
}"
#end model##############

J <- ncol(rc); I <- nrow(rc)
v <- t(rc)
lo.v <- ifelse(is.na(v)==TRUE | v==  1, 0, -5)
hi.v <- ifelse(is.na(v)==TRUE | v== -1,  0, 5)
vstar <- matrix (NA, nrow=J, ncol=I)
for (j in 1:J){
    for (i in 1:I){
        vstar[j,i] <- ifelse(v[j,i]==0, 0, ifelse(v[j,i]==1, runif(1), -1*runif(1)))}}
rm(v)
dip.parameters <- c("delta","angle", "x", "y") #, "deviance")
dip.inits.1 <- dump.format (list(v.star=vstar, x = rnorm(J), y = rnorm(J), angle = rnorm(I), delta = rnorm(I)))
dip.inits.2 <- dump.format (list(v.star=vstar, x = rnorm(J), y = rnorm(J), angle = rnorm(I), delta = rnorm(I)))
dip.inits.3 <- dump.format (list(v.star=vstar, x = rnorm(J), y = rnorm(J), angle = rnorm(I), delta = rnorm(I)))
dip.data <- dump.format (list (J=J, I=I, lo.v=lo.v, hi.v=hi.v, PAN=PAN, PRI=PRI, PRD=PRD, PT=PT))

#test ride to see program works
myWrap <- function(){
    dip.60 <- run.jags (
         model=modelSta2Dj,
         monitor=dip.parameters,
         n.chains=3,
         data=dip.data,
         inits=list( dip.inits.1, dip.inits.2, dip.inits.3 ),
         thin=1,
         burnin=5,
         sample=5,
         check.conv=FALSE,
    #     jags = "c:/Archivos de programa/JAGS/JAGS-2.2.0/bin/jags-terminal.exe",
    #     jags = "c:/Program Files (x86)/JAGS/JAGS-2.2.0/bin/jags-terminal.exe",
    #     jags = "C:/Program Files/JAGS/JAGS-3.1.0/x64/bin/jags-terminal.exe",
         plots=FALSE
    )
    return(dip.60)
}
print(system.time(myWrap()))

#to continue running in Bugs
tmp1<-list (
    v.star=vstar,
    delta=dip.60$last.values[[1]]$delta,
    angle=dip.60$last.values[[1]]$angle,
    b=dip.60$last.values[[1]]$b,
    x=dip.60$last.values[[1]]$x,
    y=dip.60$last.values[[1]]$y
    )
tmp2<-list (
    v.star=vstar,
    delta=dip.60$last.values[[2]]$delta,
    angle=dip.60$last.values[[2]]$angle,
    b=dip.60$last.values[[2]]$b,
    x=dip.60$last.values[[2]]$x,
    y=dip.60$last.values[[2]]$y
    )
tmp3<-list (
    v.star=vstar,
    delta=dip.60$last.values[[3]]$delta,
    angle=dip.60$last.values[[3]]$angle,
    b=dip.60$last.values[[3]]$b,
    x=dip.60$last.values[[3]]$x,
    y=dip.60$last.values[[3]]$y
    )
### for (chain in 1:3){dip.60$last.values[[chain]]$v.star <- vstar}
dip.60.2 <- bugs (dip.data,
                inits=list(tmp1,tmp2,tmp3),
                dip.parameters,
#                "modelSta2Dj.irt.txt", n.chains=3,
                "modelSta2Dj.txt", n.chains=3,
                n.iter=5000, n.thin=25, debug=T,
                bugs.directory = "c:/Program Files (x86)/WinBUGS14/",
#                bugs.directory = "c:/Archivos de programa/WinBUGS14/",
                program = c("WinBUGS"))

dip.60 <- dip.60.2
rm(dip.60.2)

post.draw <- rbind(dip.60$mcmc[[1]], dip.60$mcmc[[2]], dip.60$mcmc[[3]])
post.x <- post.draw[,grep("x", colnames(post.draw))]
post.y <- post.draw[,grep("y", colnames(post.draw))]

jotas <- matrix(NA, nrow=J, ncol=6)
for (j in 1:J){
    jotas[j,1] <- quantile (post.x[,j], 0.025, names=F)
    jotas[j,2] <- quantile (post.x[,j], 0.50, names=F)
    jotas[j,3] <- quantile (post.x[,j], 0.975, names=F)
    jotas[j,4] <- quantile (post.y[,j], 0.025, names=F)
    jotas[j,5] <- quantile (post.y[,j], 0.50, names=F)
    jotas[j,6] <- quantile (post.y[,j], 0.975, names=F)
    }


dipdat$color[dipdat$color=="."] <- "black"
plot(jotas[,2],jotas[,5], pch=19, col=dipdat$color)



##################################################
#### static model in 1 dimension, extremist anchor
##################################################
#cat("
#model {
#  for (j in 1:J){                ## loop over councilors
#    for (i in 1:I){              ## loop over items
#     #v.hat[j,i] ~ dbern(p[j,i])                                   ## voting rule
#     #p[j,i] <- phi(y.star[j,i])                                   ## sets 0<p<1
#     v.star[j,i] ~ dnorm(mu[j,i],1)I(lo.v[j,i],hi.v[j,i])   ## truncated normal sampling
#     mu[j,i] <- delta[i]*x[j] - n[i]                              ## utility differential
#     }
#  }
##  for (i in 1:I){
##     m[i] <- n[i] / delta[i]                                      ## midpoint
##  }
#  ## priors
#for (j in 1:(DER-1)){
#    x[j] ~  dnorm(0, .1)
#                    }
#    x[DER] ~  dnorm(-10, 4)    # Mr. RIGHT ags01p PAN
##    x[DER] <- -1
#for (j in (DER+1):(IZQ-1)){
#    x[j] ~  dnorm(0, .1)
#                          }
#    x[IZQ] ~  dnorm(10, 4)   # Mr. LEFT Noro?a PT
##    x[IZQ] <- 1
#for (j in (IZQ+1):J){
#    x[j] ~  dnorm(0, .1)
#                    }
#for (i in 1:I){
#    delta[i] ~ dnorm(0, 0.25)
#    n[i] ~ dnorm( 0, 0.25)
#              }
#}
#", file="modelSta1Dj.txt")
##
##################################################
#### static model in 1 dimension, item anchors
##################################################
#cat("
#model {
#  for (j in 1:J){                ## loop over councilors
#    for (i in 1:I){              ## loop over items
#     #y.hat[j,i] ~ dbern(p[j,i])                                   ## voting rule
#     #p[j,i] <- phi(y.star[j,i])                                   ## sets 0<p<1
#     y.star[j,i] ~ dnorm(mu[j,i],1)I(lower.y[j,i],upper.y[j,i])   ## truncated normal sampling
#     mu[j,i] <- delta[i]*x[j] - n[i]                              ## utility differential
#     }
#  }
#  for (i in 1:I){
#     m[i] <- n[i] / delta[i]                                      ## midpoint
#  }
#  ## priors
#     for (j in 1:J){
#         x[j] ~ dnorm(0, .1)
#                   }
#    for(i in 1:31){
#        delta[i] ~ dnorm( 0, 0.25)
#                  }
#    delta[32] ~ dnorm( 4, 4)      ## folio 390, right=nay
#    for(i in 33:227){
#        delta[i] ~ dnorm( 0, 0.25)
#                   }
#    delta[228] ~ dnorm(-4, 4)      ## folio 1045, right=aye
#    for(i in 229:I){
#        delta[i] ~ dnorm( 0, 0.25)
#                  }
#    for(i in 1:I){
#        n[i] ~ dnorm( 0, 0.25)
#                 }
#}
#", file="modelSta1Dj.txt")
# #
#### dynamic model for 66 members in Two Dimensions WITH CUTLINE ESTIMATES
#cat("
#model {
#  for (j in 1:J){                ## loop over diputados
#    for (i in 1:I){              ## loop over items
#      #y.hat[j,i] ~ dbern(p[j,i]);                                  ## voting rule
#      #p[j,i] <- phi(v.star[j,i]);                                  ## sets 0<p<1
#      v.star[j,i] ~ dnorm(mu[j,i],1)I(lo.v[j,i],hi.v[j,i]);   ## truncated normal sampling, cf. Jackman
#      mu[j,i] <- delta[i]*a[i]*(xOne[j]*d1[i] + xTwo[j]*d2[i] + xThree[j]*d3[i] + xFour[j]*d4[i] + xFive[j]*d5[i] + xSix[j]*d6[i] + xSeven[j]*d7[i] + xEight[j]*d8[i])
#                + delta[i]*b[i] - delta[i]*(yOne[j]*d1[i] + yTwo[j]*d2[i] + yThree[j]*d3[i] + yFour[j]*d4[i] + yFive[j]*d5[i] + ySix[j]*d6[i] + ySeven[j]*d7[i] + yEight[j]*d8[i])  ## utility differential
#                  }
#      xOne[j] ~   dnorm (xZero[j],15);  ## en 2do intento slack era 20, 3ro 10
#      xTwo[j] ~   dnorm (xOne[j],15);
#      xThree[j] ~ dnorm (xTwo[j],15);
#      xFour[j] ~  dnorm (xThree[j],15);
#      xFive[j] ~  dnorm (xFour[j],15);
#      xSix[j] ~   dnorm (xFive[j],15);
#      xSeven[j] ~ dnorm (xSix[j],15);
#      xEight[j] ~ dnorm (xSeven[j],15);
#      yOne[j] ~   dnorm (yZero[j],15);
#      yTwo[j] ~   dnorm (yOne[j],15);
#      yThree[j] ~ dnorm (yTwo[j],15);
#      yFour[j] ~  dnorm (yThree[j],15);
#      yFive[j] ~  dnorm (yFour[j],15);
#      ySix[j] ~   dnorm (yFive[j],15);
#      ySeven[j] ~ dnorm (ySix[j],15);
#      yEight[j] ~ dnorm (ySeven[j],15);
#                }
#  for (i in 1:I){
#  a[i] <- sin(angle[i]) / sqrt(1-sin(angle[i])*sin(angle[i]))
#  }
#  ################
#  ## priors
#  ################
#for (j in 1:(N-1)){
#    xZero[j] ~  dnorm(0, 1)
#    yZero[j] ~  dnorm(0, 1)
#    }
#    xZero[N] ~  dnorm(0, 4)    # Mrs. NORTH Pi?a Olmedo Laura (PRD)
#    yZero[N] ~  dnorm(2, 4)
##    xZero[N] <- 0
##    yZero[N] <- 2
#for (j in (N+1):(W-1)){
#    xZero[j] ~  dnorm(0, 1)
#    yZero[j] ~  dnorm(0, 1)
#    }
#    xZero[W] ~  dnorm(-2, 4)   # Mr. WEST M?ndez Rangel Avelino (PRD)
#    yZero[W] ~  dnorm(0, 4)
##    xZero[W] <- -2
##    yZero[W] <- 0
#for (j in (W+1):(E-1)){
#    xZero[j] ~  dnorm(0, 1)
#    yZero[j] ~  dnorm(0, 1)
#    }
#    xZero[E] ~  dnorm(2, 4)    # Mrs. EAST Paula Adriana Soto Maldonado (PAN)
#    yZero[E] ~  dnorm(0, 4)
##    xZero[E] <- 2
##    yZero[E] <- 0
#for (j in (E+1):(S-1)){
#    xZero[j] ~  dnorm(0, 1)
#    yZero[j] ~  dnorm(0, 1)
#    }
#    xZero[S] ~  dnorm(0, 4)    # Mr. SOUTH Tenorio Antiga Xiuh (PANAL)
#    yZero[S] ~  dnorm(-2, 4)
##    xZero[S] <- 0
##    yZero[S] <- -2
#for (j in (S+1):J){
#    xZero[j] ~  dnorm(0, 1)
#    yZero[j] ~  dnorm(0, 1)
#    }
#    for(i in 1:I){
#        delta[i] ~ dnorm( 0, 0.01)
#        angle[i] ~ dunif(-1.57,1.57) # (-pi/2,pi/2)
#        b[i] ~ dnorm( 0, .01)
#                 }
#}
#", file="model66Dyn2Dj.txt")
##
##
####################################################################
#### static model for 66 members in Two Dimensions, four ITEM anchors WITH CUTLINE ESTIMATES
####################################################################
#cat("
#model {
#  for (j in 1:J){                ## loop over diputados
#    for (i in 1:I){              ## loop over items
#      #y.hat[j,i] ~ dbern(p[j,i]);                                  ## voting rule
#      #p[j,i] <- phi(v.star[j,i]);                                  ## sets 0<p<1
#      v.star[j,i] ~ dnorm(mu[j,i],1)I(lo.v[j,i],hi.v[j,i]);   ## truncated normal sampling
#      mu[j,i] <- delta[i]*a[i]*x[j] + delta[i]*b[i] - delta[i]*y[j] ## utility differential
#                  }
#                }
#  for (i in 1:I){
#  a[i] <- sin(angle[i]) / sqrt(1-sin(angle[i])*sin(angle[i]))
#  }
#  ## priors ################
#for (j in 1:J){
#    x[j] ~  dnorm(0, 1)
#    y[j] ~  dnorm(0, 1)
#    }
#for(i in 1:(V1-1)){
#    delta[i] ~ dnorm( 0, 0.25)
##    angle[i] ~ dunif(-1.57,1.57) # (-pi/2,pi/2)
#    angle[i] ~ dunif(.392,1.178) # (pi/8,3pi/8)
#    b[i] ~ dnorm( 0, .25)
#    }
#delta[V1] ~ dnorm( -4, 4)
#angle[V1] ~ dunif(1.37,1.77) # (7pi/16,9pi/16) ---- VERTICAL
#b[V1] ~ dnorm( 0, 4)
#for(i in (V1+1):(H1-1)){
#    delta[i] ~ dnorm( 0, 0.25)
##    angle[i] ~ dunif(-1.57,1.57) # (-pi/2,pi/2)
#    angle[i] ~ dunif(.392,1.178) # (pi/8,3pi/8)
#    b[i] ~ dnorm( 0, .25)
#    }
#delta[H1] ~ dnorm( -4, 4)
#angle[H1] ~ dunif(-0.2,0.2) # (-pi/16,pi/16) ------ HORIZONTAL
#b[H1] ~ dnorm( 0, 4)
#for(i in (H1+1):(V2-1)){
#    delta[i] ~ dnorm( 0, 0.25)
##    angle[i] ~ dunif(-1.57,1.57) # (-pi/2,pi/2)
#    angle[i] ~ dunif(.392,1.178) # (pi/8,3pi/8)
#    b[i] ~ dnorm( 0, .25)
#    }
#delta[V2] ~ dnorm( 4, 4)
#angle[V2] ~ dunif(1.37,1.77) # (7pi/16,9pi/16) ---- VERTICAL
#b[V2] ~ dnorm( 0, 4)
#for(i in (V2+1):(H2-1)){
#    delta[i] ~ dnorm( 0, 0.25)
##    angle[i] ~ dunif(-1.57,1.57) # (-pi/2,pi/2)
#    angle[i] ~ dunif(.392,1.178) # (pi/8,3pi/8)
#    b[i] ~ dnorm( 0, .25)
#    }
#delta[H2] ~ dnorm( -4, 4)
#angle[H2] ~ dunif(-0.2,0.2) # (-pi/16,pi/16) ------ HORIZONTAL
#b[H2] ~ dnorm( 0, 4)
#for(i in (H2+1):I){
#    delta[i] ~ dnorm( 0, 0.25)
##    angle[i] ~ dunif(-1.57,1.57) # (-pi/2,pi/2)
#    angle[i] ~ dunif(.392,1.178) # (pi/8,3pi/8)
#    b[i] ~ dnorm( 0, .25)
#    }
#}
#", file="model66Sta2Di4.txt")
##
####################################################################
#### static model for 66 members in Two Dimensions, two ITEM anchors WITH CUTLINE ESTIMATES
####################################################################
#cat("
#model {
#  for (j in 1:J){                ## loop over diputados
#    for (i in 1:I){              ## loop over items
#      #y.hat[j,i] ~ dbern(p[j,i]);                                  ## voting rule
#      #p[j,i] <- phi(v.star[j,i]);                                  ## sets 0<p<1
#      v.star[j,i] ~ dnorm(mu[j,i],1)I(lo.v[j,i],hi.v[j,i]);   ## truncated normal sampling
#      mu[j,i] <- delta[i]*a[i]*x[j] + delta[i]*b[i] - delta[i]*y[j] ## utility differential
#                  }
#                }
#  for (i in 1:I){
#  a[i] <- sin(angle[i]) / sqrt(1-sin(angle[i])*sin(angle[i]))
#  }
#  ## priors ################
#for (j in 1:J){
#    x[j] ~  dnorm(0, 1)
#    y[j] ~  dnorm(0, 1)
#    }
#for(i in 1:(V1-1)){
#    delta[i] ~ dnorm( 0, 0.25)
##    angle[i] ~ dunif(-1.57,1.57) # (-pi/2,pi/2)
#    angle[i] ~ dunif(.392,1.178) # (pi/8,3pi/8)
#    b[i] ~ dnorm( 0, .25)
#    }
#delta[V1] ~ dnorm( -4, 4)
#angle[V1] ~ dunif(1.37,1.77) # (7pi/16,9pi/16) ---- VERTICAL
#b[V1] ~ dnorm( 0, 4)
#for(i in (V1+1):(H1-1)){
#    delta[i] ~ dnorm( 0, 0.25)
##    angle[i] ~ dunif(-1.57,1.57) # (-pi/2,pi/2)
#    angle[i] ~ dunif(.392,1.178) # (pi/8,3pi/8)
#    b[i] ~ dnorm( 0, .25)
#    }
#delta[H1] ~ dnorm( -4, 4)
#angle[H1] ~ dunif(-0.2,0.2) # (-pi/16,pi/16) ------ HORIZONTAL
#b[H1] ~ dnorm( 0, 4)
#for(i in (H1+1):I){
#    delta[i] ~ dnorm( 0, 0.25)
##    angle[i] ~ dunif(-1.57,1.57) # (-pi/2,pi/2)
#    angle[i] ~ dunif(.392,1.178) # (pi/8,3pi/8)
#    b[i] ~ dnorm( 0, .25)
#    }
#}
#", file="model66Sta2Di2.txt")
##
#### dynamic model for 66 members in Two Dimensions -- IRT PARAMETERIZATION
#cat("
#model {
#  for (j in 1:J){                ## loop over diputados
#    for (i in 1:I){              ## loop over items
#      #v.hat[j,i] ~ dbern(p[j,i]);                                  ## voting rule
#      #p[j,i] <- phi(v.star[j,i]);                                  ## sets 0<p<1
#      v.star[j,i] ~ dnorm(mu[j,i],1)I(lo.v[j,i],hi.v[j,i]);   ## truncated normal sampling
#      mu[j,i] <- beta[i]*(xOne[j]*d1[i] + xTwo[j]*d2[i] + xThree[j]*d3[i] + xFour[j]*d4[i] + xFive[j]*d5[i] + xSix[j]*d6[i] + xSeven[j]*d7[i] + xEight[j]*d8[i])
#                - alpha[i] + delta[i]*(yOne[j]*d1[i] + yTwo[j]*d2[i] + yThree[j]*d3[i] + yFour[j]*d4[i] + yFive[j]*d5[i] + ySix[j]*d6[i] + ySeven[j]*d7[i] + yEight[j]*d8[i])  ## utility differential
#                  }
#      xOne[j] ~   dnorm (xZero[j],15);  ## en 2do intento slack era 20, 3ro 10
#      xTwo[j] ~   dnorm (xOne[j],15);
#      xThree[j] ~ dnorm (xTwo[j],15);
#      xFour[j] ~  dnorm (xThree[j],15);
#      xFive[j] ~  dnorm (xFour[j],15);
#      xSix[j] ~   dnorm (xFive[j],15);
#      xSeven[j] ~ dnorm (xSix[j],15);
#      xEight[j] ~ dnorm (xSeven[j],15);
#      yOne[j] ~   dnorm (yZero[j],15);
#      yTwo[j] ~   dnorm (yOne[j],15);
#      yThree[j] ~ dnorm (yTwo[j],15);
#      yFour[j] ~  dnorm (yThree[j],15);
#      yFive[j] ~  dnorm (yFour[j],15);
#      ySix[j] ~   dnorm (yFive[j],15);
#      ySeven[j] ~ dnorm (ySix[j],15);
#      yEight[j] ~ dnorm (ySeven[j],15);
#                }
### ESTO LO PUEDO SACAR POST ESTIMACION
###  for (i in 1:I){
###  a[i] <- beta[i] / delta[i]  ## pendiente de cutline
###  b[i] <- alpha[i] / delta[i] ## constante de cutline
###  }
#  ################
#  ## priors
#  ################
### 1a dim ############
#for (j in 1:(N-1)){
#    xZero[j] ~  dnorm(0, 1)
#    }
#    xZero[N] ~  dnorm(0, 4)    # Mrs. NORTH Pi?a Olmedo Laura (PRD)
##    xZero[N] <- 0
#for (j in (N+1):(W-1)){
#    xZero[j] ~  dnorm(0, 1)
#    }
#    xZero[W] ~  dnorm(-2, 4)    # Mr. WEST M?ndez Rangel Avelino (PRD)
##    xZero[W] <- -2
#for (j in (W+1):(E-1)){
#    xZero[j] ~  dnorm(0, 1)
#    }
#    xZero[E] ~  dnorm(2, 4)    # Mrs. EAST Paula Adriana Soto Maldonado (PAN)
##    xZero[E] <- 2
#for (j in (E+1):(S-1)){
#    xZero[j] ~  dnorm(0, 1)
#    }
#    xZero[S] ~  dnorm(0, 4)    # Mr. SOUTH Tenorio Antiga Xiuh (PANAL)
##    xZero[S] <- 0
#for (j in (S+1):J){
#    xZero[j] ~  dnorm(0, 1)
#    }
### 2a dim  ############
#for (j in 1:(N-1)){
#    yZero[j] ~  dnorm(0, 1)
#    }
#    yZero[N] ~  dnorm(2, 4)    # Mrs. NORTH
##    yZero[N] <- 2
#for (j in (N+1):(W-1)){
#    yZero[j] ~  dnorm(0, 1)
#    }
#    yZero[W] ~  dnorm(0, 4)    # Mr. WEST
##    yZero[W] <- 0
#for (j in (W+1):(E-1)){
#    yZero[j] ~  dnorm(0, 1)
#    }
#    yZero[E] ~  dnorm(0, 4)    # Mrs. EAST
##    yZero[E] <- 0
#for (j in (E+1):(S-1)){
#    yZero[j] ~  dnorm(0, 1)
#    }
#    yZero[S] ~  dnorm(-2, 4)    # Mr. SOUTH
##    yZero[S] <- -2
#for (j in (S+1):J){
#    yZero[j] ~  dnorm(0, 1)
#    }
#    for(i in 1:I){
#        alpha[i] ~ dnorm( 0, 1)
#        beta[i]  ~ dnorm( 0, 1)
#        delta[i] ~ dnorm( 0, 1)
#                 }
#}
#", file="model66Dyn2Dj.irt.txt")
##
##
########################################################################################
#### dynamic model for 66 members in Two Dimensions Four Item anchors-- IRT PARAMETERIZATION
########################################################################################
#cat("
#model {
#  for (j in 1:J){                ## loop over diputados
#    for (i in 1:I){              ## loop over items
#      #v.hat[j,i] ~ dbern(p[j,i]);                                  ## voting rule
#      #p[j,i] <- phi(v.star[j,i]);                                  ## sets 0<p<1
#      v.star[j,i] ~ dnorm(mu[j,i],1)I(lo.v[j,i],hi.v[j,i]);   ## truncated normal sampling
#      mu[j,i] <- beta[i]*(xOne[j]*d1[i] + xTwo[j]*d2[i] + xThree[j]*d3[i]
#                 + xFour[j]*d4[i] + xFive[j]*d5[i] + xSix[j]*d6[i] + xSeven[j]*d7[i]
#                 + xEight[j]*d8[i])
#                 - alpha[i] + delta[i]*(yOne[j]*d1[i] + yTwo[j]*d2[i] + yThree[j]*d3[i]
#                 + yFour[j]*d4[i] + yFive[j]*d5[i] + ySix[j]*d6[i] + ySeven[j]*d7[i]
#                 + yEight[j]*d8[i])  ## utility differential
#                  }
#      xOne[j] ~   dnorm (xZero[j],15);  ## en 2do intento slack era 20, 3ro 10
#      xTwo[j] ~   dnorm (xOne[j],15);
#      xThree[j] ~ dnorm (xTwo[j],15);
#      xFour[j] ~  dnorm (xThree[j],15);
#      xFive[j] ~  dnorm (xFour[j],15);
#      xSix[j] ~   dnorm (xFive[j],15);
#      xSeven[j] ~ dnorm (xSix[j],15);
#      xEight[j] ~ dnorm (xSeven[j],15);
#      yOne[j] ~   dnorm (yZero[j],15);
#      yTwo[j] ~   dnorm (yOne[j],15);
#      yThree[j] ~ dnorm (yTwo[j],15);
#      yFour[j] ~  dnorm (yThree[j],15);
#      yFive[j] ~  dnorm (yFour[j],15);
#      ySix[j] ~   dnorm (yFive[j],15);
#      ySeven[j] ~ dnorm (ySix[j],15);
#      yEight[j] ~ dnorm (ySeven[j],15);
#                }
### ESTO LO PUEDO SACAR POST ESTIMACION
###  for (i in 1:I){
###  a[i] <- beta[i] / delta[i]  ## pendiente de cutline
###  b[i] <- alpha[i] / delta[i] ## constante de cutline
###  }
#  ################
#  ## priors
#  ################
### 1a dim ############
#for (j in 1:(N-1)){
#    xZero[j] ~  dnorm(0, 1)
#    yZero[j] ~  dnorm(0, 1)
#    }
#    xZero[N] ~  dnorm(-2, 4)    # Mrs. NORTH Pi?a Olmedo Laura (PRD)
#    yZero[N] ~  dnorm(2, 4)
##    xZero[N] <- 0
##    yZero[N] <- 2
#for (j in (N+1):J){
#    xZero[j] ~  dnorm(0, 1)
#    yZero[j] ~  dnorm(0, 1)
#    }
#for(i in 1:(V1-1)){
#    alpha[i] ~ dnorm( 0, 1)
#    beta[i]  ~ dnorm( 0, 1)
#    delta[i] ~ dnorm( 0, 1)
#    }
#alpha[V1] ~ dnorm( 0, 1)
#beta[V1]  ~ dnorm(-4, 20)
#delta[V1] ~ dnorm(-4, 20)
#for(i in (V1+1):(H1-1)){
#    alpha[i] ~ dnorm( 0, 1)
#    beta[i]  ~ dnorm( 0, 1)
#    delta[i] ~ dnorm( 0, 1)
#    }
#alpha[H1] ~ dnorm( 0, 1)
#beta[H1]  ~ dnorm( 4, 20)
#delta[H1] ~ dnorm(-4, 20)
#for(i in (H1+1):(V2-1)){
#    alpha[i] ~ dnorm( 0, 1)
#    beta[i]  ~ dnorm( 0, 1)
#    delta[i] ~ dnorm( 0, 1)
#    }
#alpha[V2] ~ dnorm( 0, 1)
#beta[V2]  ~ dnorm( 4, 20)
#delta[V2] ~ dnorm( 4, 20)
#for(i in (V2+1):(H2-1)){
#    alpha[i] ~ dnorm( 0, 1)
#    beta[i]  ~ dnorm( 0, 1)
#    delta[i] ~ dnorm( 0, 1)
#    }
#alpha[H2] ~ dnorm( 0, 1)
#beta[H2]  ~ dnorm( 4, 20)
#delta[H2] ~ dnorm(-4, 20)
#for(i in (H2+1):I){
#    alpha[i] ~ dnorm( 0, 1)
#    beta[i]  ~ dnorm( 0, 1)
#    delta[i] ~ dnorm( 0, 1)
#    }
#}
#", file="model66Dyn2Di4.irt.txt")
##
##########################################################################################



### WAS NEEDED IN ALDF
#for (n in
#1:ncol(rc)){
#    rc[,n]<-as.numeric(rc[,n])
#}
#
### SORTS BY DATE
#tmp<-RCs[order(RCs$yr, RCs$mo, RCs$dy, RCs$folio),]
#RCs<-tmp
#
## ## WILL BE NEEDED IN DYNAMIC VERSION, IF AT ALL
## trim <- votdat$mo
## trim[trim==1 | trim==2 | trim==3]<- 1
## trim[trim==4 | trim==5 | trim==6]<- 2
## trim[trim==7 | trim==8 | trim==9]<- 3
## trim[trim==10 | trim==11 | trim==12]<- 4
## #
## cuad <- votdat$mo
## cuad[cuad==1 | cuad==2 | cuad==3 | cuad==4]<- 1
## cuad[cuad==5 | cuad==6 | cuad==7 | cuad==8]<- 2
## cuad[cuad==9 | cuad==10 | cuad==11 | cuad==12]<- 3
## #
## titCuad <- cuad
## titCuad[cuad==1]<-"2006-3"
## titCuad[cuad==2]<-"2007-1"
## titCuad[cuad==3]<-"2007-2"
## titCuad[cuad==4]<-"2007-3"
## titCuad[cuad==5]<-"2008-1"
## titCuad[cuad==6]<-"2008-2"
## titCuad[cuad==7]<-"2008-3"
## titCuad[cuad==8]<-"2009-1"
## #
## sem <- votdat$mo
## sem[sem>0 & sem<7]<- 1
## sem[sem>6 & sem<13]<- 2
## #
## # unstar appropriate
## #tmp<-(votdat$yr-2006)*4 ## to work with trimestres
## tmp<-(votdat$yr-2006)*3 ## to work with cuadrimestres
## #tmp<-(votdat$yr-2006)*2 ## to work with semestres
## #trim<-trim+tmp
## #trim<-trim-min(trim)+1
## cuad<-cuad+tmp
## cuad<-cuad-min(cuad)+1
## #sem<-sem+tmp
## #sem<-sem-min(sem)+1
## #
## #T<-max(trim)
## T<-max(cuad)
## #T<-max(sem)

## ONE-DIM ARRANGEMENT
## AGREEMENT MATRIX --- LA GUARDE PORQUE ESTO TARDA A?OS
load("agreeMatrix.Rdata")
#votes <- rc
#votes[votes==0] <- -1  # los 0s se vuelven -1s # DEJA ABSTENCION == NAY
#I <- dim(votes)[1]; J <- dim(votes)[2]
#agreeMatrix <- matrix(NA, ncol=J, nrow=J); tmp <- rep(NA, times=I)
#for (j in 1:J){
#    agreeMatrix[j,j] <- 1  ## DIAGONAL
#              }
#for (j1 in 2:J){
#    for (j2 in (j1-1):1){
#        for (i in 1:I){
#            tmp[i] <- ifelse(votes[i,j1]==votes[i,j2], 1, 0)
#                      }
#        agreeMatrix[j2,j1] <- sum(tmp)/I; agreeMatrix[j1,j2] <- agreeMatrix[j2,j1]
#        print( paste("j1 =",j1,"; j2 =",j2) )
#                        }
#               }
## SQUARED DISTANCES
sd <- (1-agreeMatrix)^2
## DOUBLE-CENTRED MATRIX
pmean <- rep(NA, times=J); mmat <- mean(sd); dc <- sd
for (j in 1:J){
    pmean[j] <- mean(sd[j,])
              }
for (r in 1:J){
    for (c in 1:J){
        dc[r,c] <- (sd[r,c] - pmean[r] - pmean[c] + mmat)/-2
                  }
              }
## SIMPLE ONE-DIM IDEAL POINTS
tmp <- sqrt(dc[1,1])
ip  <- c(tmp, dc[2:J,1]/tmp)
##
## EXTREMA DERECHA
thr <- .14
data.frame(ip=ip[c(1:J)[ip>thr]], id=dipdat$id[c(1:J)[ip>thr]], nom=dipdat$nom[c(1:J)[ip>thr]], part=dipdat$part[c(1:J)[ip>thr]], noVote=dipdat$noVoteRate[c(1:J)[ip>thr]])
##EXTREMA IZQUIERDA
thr <- -.185
data.frame(ip=ip[c(1:J)[ip<thr]], id=dipdat$id[c(1:J)[ip<thr]], nom=dipdat$nom[c(1:J)[ip<thr]], part=dipdat$part[c(1:J)[ip<thr]], noVote=dipdat$noVoteRate[c(1:J)[ip< thr]])
##
plot(c(-.3,.3), c(1,7), type="n")
for (j in 1:J){
    points(ip[j], dipdat$part[j], pch=20,col=dipdat$color[j])
    }


##################################################
### FACTOR ANALYSIS TO ESTIMATE DIMENSIONALITY ###
##################################################
#
### ALLOWS TO DROP CASES FROM ANALYSIS
#year1 <- ifelse( (cuad==1 | cuad==2 | cuad==3), 1, 0 ) ## FALTA QUITAR A UN DIPUTADO (M?S?) QUE NO ENTR? HASTA DESPU?S
#drop <- ifelse(year1==0,1,0)
#tmp<-RCs[drop==0,]
#RCs<-tmp
#sem<-sem[drop==0]; cuad<-cuad[drop==0]; trim<-trim[drop==0]
#
votes <- rc
votes[votes==-1] <- 0  # los -1s se vuelven 0s # DEJA ABSTENCION == NAY
votes <- t(votes)
votes <- votes[,1:50] ## subset to test
cor(votes)
factanal(votes, factors=4) # varimax is the default
#
factanal(votes, factors=5, rotation="promax")
#
# A little demonstration, v2 is just v1 with noise,
# and same for v4 vs. v3 and v6 vs. v5
# Last four cases are there to add noise
# and introduce a positive manifold (g factor)
v1 <- c(1,1,1,1,1,1,1,1,1,1,3,3,3,3,3,4,5,6)
v2 <- c(1,2,1,1,1,1,2,1,2,1,3,4,3,3,3,4,6,5)
v3 <- c(3,3,3,3,3,1,1,1,1,1,1,1,1,1,1,5,4,6)
v4 <- c(3,3,4,3,3,1,1,2,1,1,1,1,2,1,1,5,6,4)
v5 <- c(1,1,1,1,1,3,3,3,3,3,1,1,1,1,1,6,4,5)
v6 <- c(1,1,1,2,1,3,3,3,4,3,1,1,1,2,1,6,5,4)
m1 <- cbind(v1,v2,v3,v4,v5,v6)
cor(m1)
factanal(m1, factors=3) # varimax is the default
factanal(m1, factors=3, rotation="promax")
# The following shows the g factor as PC1
prcomp(m1)
#
## formula interface
factanal(~v1+v2+v3+v4+v5+v6, factors = 3,
        scores = "Bartlett")$scores
#
## a realistic example from Barthlomew (1987, pp. 61-65)
example(ability.cov)


eric   ###################################################
###       Static 2Dimensions party anchors      ###
###################################################

## ANCHORS
#           Encinas                      Chepina                     Rojas
#NW<-grep("dfrp12p", dipdat$id); NE<-grep("mexrp01p", dipdat$id); S<-grep("mexrp07p", dipdat$id)
#          AgsPan                      Noro?a                     Rojas
#NE<-grep("ags01p", dipdat$id); SW<-grep("df19p", dipdat$id); S<-grep("mexrp07p", dipdat$id)
#tmp<-ifelse( (RCs$folio==2  & RCs$yr==2006 & RCs$mo==11 & RCs$dy==9 ),1,0 ); V1<-grep(1,tmp) # Vertical 8
#tmp<-ifelse( (RCs$folio==6  & RCs$yr==2006 & RCs$mo==12 & RCs$dy==28),1,0 ); V2<-grep(1,tmp) # Vertical 66
#tmp<-ifelse( (RCs$folio==10 & RCs$yr==2006 & RCs$mo==12 & RCs$dy==26),1,0 ); H1<-grep(1,tmp) # Horizontal 57

### ALLOWS TO DROP CASES FROM ANALYSIS
#year1 <- ifelse( (cuad==1 | cuad==2 | cuad==3), 1, 0 ) ## FALTA QUITAR A UN DIPUTADO (M?S?) QUE NO ENTR? HASTA DESPU?S
#drop <- ifelse(year1==0,1,0)
#tmp<-rc[drop==0,]
#rc<-tmp
#sem<-sem[drop==0]; cuad<-cuad[drop==0]; trim<-trim[drop==0]

## SORT BY PARTY
tmp <- 1:dim(rc)[1]
tmp <- ifelse ( dipdat$part=="pan", tmp,
        ifelse (dipdat$part=="pri", tmp+1000,
         ifelse ( dipdat$part=="prd", tmp+2000,
          ifelse ( dipdat$part=="pt", tmp+3000, tmp+4000))))
## rcold <- rc; dipold <- dipdat
rc <- rc[,order(tmp)]; dipdat <- dipdat[order(tmp),]
## PARTY INDICES
PAN <- length(tmp[tmp<1001]); PRI <- PAN+length(tmp[tmp>1000&tmp<2001]);
PRD <- PRI+length(tmp[tmp>2000&tmp<3001]); PT <- PRD+length(tmp[tmp>3000&tmp<4001])

v <- rc
v <- t(v)
J <- nrow(v); I <- ncol(v)
lo.v <- ifelse(is.na(v)==TRUE | v== 1, 0, -5)
hi.v <- ifelse(is.na(v)==TRUE | v==-1,  0, 5)
vstar <- matrix (NA, nrow=J, ncol=I)
for (j in 1:J){
for (i in 1:I){
  vstar[j,i] <- ifelse(v[j,i]==0, 0, ifelse(v[j,i]==1, runif(1), -1*runif(1)))}}
rm(v)
dip.data <- list ("J", "I", "lo.v", "hi.v", "PAN", "PRI", "PRD", "PT")
dip.inits <- function (){
    list (
    v.star=vstar,
    delta=rnorm(I),
    angle=runif(I),
    b=rnorm(I),
    x=rnorm(J),
    y=rnorm(J)
    )
    }
dip.parameters <- c("delta","angle", "b", "x", "y")

#test ride to see program works
dip.60 <- bugs (dip.data, dip.inits, dip.parameters,
#                "modelSta2Dj.irt.txt", n.chains=3,
                "modelSta2Dj.txt", n.chains=3,
                n.iter=10, n.thin=1, debug=T,
                bugs.directory = "c:/Program Files (x86)/WinBUGS14/",
#                bugs.directory = "c:/Archivos de programa/WinBUGS14/",
                program = c("WinBUGS"))

#longer run
dip.60 <- bugs (dip.data, dip.inits, dip.parameters,
#                "modelSta2Dj.irt.txt", n.chains=3,
                "modelSta2Dj.txt", n.chains=1,
                n.iter=5000, n.thin=25, debug=F,
                bugs.directory = "c:/Program Files (x86)/WinBUGS14/",
#                bugs.directory = "c:/Archivos de programa/WinBUGS14/",
                program = c("WinBUGS"))

plot(dip.60)
print(dip.60)

#to continue running
tmp1<-list (
    v.star=vstar,
    delta=dip.60$last.values[[1]]$delta,
    angle=dip.60$last.values[[1]]$angle,
    b=dip.60$last.values[[1]]$b,
    x=dip.60$last.values[[1]]$x,
    y=dip.60$last.values[[1]]$y
    )
tmp2<-list (
    v.star=vstar,
    delta=dip.60$last.values[[2]]$delta,
    angle=dip.60$last.values[[2]]$angle,
    b=dip.60$last.values[[2]]$b,
    x=dip.60$last.values[[2]]$x,
    y=dip.60$last.values[[2]]$y
    )
tmp3<-list (
    v.star=vstar,
    delta=dip.60$last.values[[3]]$delta,
    angle=dip.60$last.values[[3]]$angle,
    b=dip.60$last.values[[3]]$b,
    x=dip.60$last.values[[3]]$x,
    y=dip.60$last.values[[3]]$y
    )
### for (chain in 1:3){dip.60$last.values[[chain]]$v.star <- vstar}
dip.60.2 <- bugs (dip.data,
                inits=list(tmp1,tmp2,tmp3),
                dip.parameters,
#                "modelSta2Dj.irt.txt", n.chains=3,
                "modelSta2Dj.txt", n.chains=3,
                n.iter=5000, n.thin=25, debug=T,
                bugs.directory = "c:/Program Files (x86)/WinBUGS14/",
#                bugs.directory = "c:/Archivos de programa/WinBUGS14/",
                program = c("WinBUGS"))

dip.60 <- dip.60.2
rm(dip.60.2)

plot(dip.60)
print(dip.60)

attach.bugs(dip.60)

jotas <- matrix(NA, nrow=J, ncol=6)
for (j in 1:J){
    jotas[j,1] <- quantile (x[,j], 0.025, names=F)
    jotas[j,2] <- quantile (x[,j], 0.50, names=F)
    jotas[j,3] <- quantile (x[,j], 0.975, names=F)
    jotas[j,4] <- quantile (y[,j], 0.025, names=F)
    jotas[j,5] <- quantile (y[,j], 0.50, names=F)
    jotas[j,6] <- quantile (y[,j], 0.975, names=F)
    }

dipdat$color[dipdat$color=="."] <- "black"
plot(jotas[,2],jotas[,5], pch=19, col=dipdat$color)

bes <- matrix(NA, nrow=I, ncol=3)
for (i in 1:I){
    bes[i,1] <- quantile (b[,i], 0.025, names=F)
    bes[i,2] <- quantile (b[,i], 0.50, names=F)
    bes[i,3] <- quantile (b[,i], 0.975, names=F)
    }

a <- sin(angle) / sqrt(1-sin(angle)*sin(angle))
as <- matrix(NA, nrow=I, ncol=3)
for (i in 1:I){
    as[i,1] <- quantile (a[,i], 0.025, names=F)
    as[i,2] <- quantile (a[,i], 0.50, names=F)
    as[i,3] <- quantile (a[,i], 0.975, names=F)
    }
angles <- matrix(NA, nrow=I, ncol=3)
for (i in 1:I){
    angles[i,1] <- quantile (angle[,i], 0.025, names=F)
    angles[i,2] <- quantile (angle[,i], 0.50, names=F)
    angles[i,3] <- quantile (angle[,i], 0.975, names=F)
    }

subset <- c(7,41,121,160)
plot(jotas[,2],jotas[,5],pch=19,col=dipdat$color)
for (i in subset){
    abline(a=bes[i,2], b=as[i,2])}

a <- -beta / delta ## pendiente cutline
b <- alpha / delta ## constante cutline
## 45-DEGREE CLOCKWISE ROTATION OF COORDINATES (SO THAT PRIORS REMAIN n, s, e, w)
xR <- x*cos(pi/4) + y*sin(pi/4)
yR <- -x*sin(pi/4) + y*cos(pi/4)
x <- xR; y <- yR;
## 45-DEGREE CLOCKWISE ROTATION OF CUTLINES
xA <- -b/a; yA <- rep(0, length(a)); xO <- rep(0, length(a)); yO <- b  ## coords de Abscisa al origen y Ordenada al origan de c/cutline
xAR <- xA*cos(pi/4) + yA*sin(pi/4)
yAR <- -xA*sin(pi/4) + yA*cos(pi/4)
xOR <- xO*cos(pi/4) + yO*sin(pi/4)
yOR <- -xO*sin(pi/4) + yO*cos(pi/4)
X <- xAR; Y <- yAR; XX <- xOR; YY <- yOR ## simplifica notaci?n
aR <- (YY-Y)/(XX-X)           ## pendiente del cutline rotado
bR <- YY -((YY-Y)/(XX-X))*XX  ## constante del cutline rotado
rm(X,Y,XX,YY)
a <- aR; b <- bR


jotas <- matrix(NA, nrow=J, ncol=6)
for (j in 1:J){
    jotas[j,1] <- quantile (x[,j], 0.025, names=F)
    jotas[j,2] <- quantile (x[,j], 0.50, names=F)
    jotas[j,3] <- quantile (x[,j], 0.975, names=F)
    jotas[j,4] <- quantile (y[,j], 0.025, names=F)
    jotas[j,5] <- quantile (y[,j], 0.50, names=F)
    jotas[j,6] <- quantile (y[,j], 0.975, names=F)
    }

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

#######################################################
#######################################################
###      Static 1Dimension two extremist anchors    ###
#######################################################
#######################################################

## ANCHORS
##          AgsPan                      Noro?a
#DER<-grep("ags01p", dipdat$id); IZQ<-grep("df19p", dipdat$id)
##          Luken                      C?rdenas
DER<-grep("bc05p", dipdat$id); IZQ<-grep("df04p", dipdat$id)

### ALLOWS TO DROP CASES FROM ANALYSIS
#year1 <- ifelse( (cuad==1 | cuad==2 | cuad==3), 1, 0 ) ## FALTA QUITAR A UN DIPUTADO (M?S?) QUE NO ENTR? HASTA DESPU?S
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
ips <- data.frame(p025=tmp, p50=tmp, p975=tmp, nom=dipdat$nom, part=dipdat$part, id=dipdat$id)
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
X <- xAR; Y <- yAR; XX <- xOR; YY <- yOR ## simplifica notaci?n
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
for (t in 8) jotas[t,,65] <- rep(NA,6) ## Jorge D?az Cuervo pide licencia 23/9/2008=inicio cuad 7
for (t in 1:6) jotas[t,,66] <- rep(NA,6) ## Carla S?nchez Armas, la suplente

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
#year1 <- ifelse( (cuad==1 | cuad==2 | cuad==3), 1, 0 ) ## FALTA QUITAR A UN DIPUTADO (M?S?) QUE NO ENTR? HASTA DESPU?S
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

