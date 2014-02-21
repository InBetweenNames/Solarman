{-# LANGUAGE NoMonomorphismRestriction #-}
module SolarmanTriplestore where

import Getts
import Data.List as List
import qualified Data.Set as Set
import AGParser2
import TypeAg2


dataStore =
	---------------- orbit relation -------------
	[("event1000", "subject", "mercury"),
	("event1000", "type", "orbit_ev"),
	("event1000", "object", "sol"),
	("event1001", "subject", "venus"),
	("event1001", "type", "orbit_ev"),
	("event1001", "object", "sol"),
	("event1002", "subject", "earth"),
	("event1002", "type", "orbit_ev"),
	("event1002", "object", "sol"),
	("event1003", "subject", "mars"),
	("event1003", "type", "orbit_ev"),
	("event1003", "object", "sol"),
	("event1004", "subject", "jupiter"),
	("event1004", "type", "orbit_ev"),
	("event1004", "object", "sol"),
	("event1005", "subject", "saturn"),
	("event1005", "type", "orbit_ev"),
	("event1005", "object", "sol"),
	("event1006", "subject", "uranus"),
	("event1006", "type", "orbit_ev"),
	("event1006", "object", "sol"),
	("event1007", "subject", "neptune"),
	("event1007", "type", "orbit_ev"),
	("event1007", "object", "sol"),
	("event1008", "subject", "pluto"),
	("event1008", "type", "orbit_ev"),
	("event1008", "object", "sol"),
	("event1009", "subject", "luna"),
	("event1009", "type", "orbit_ev"),
	("event1009", "object", "earth"),
	("event1010", "subject", "phobos"),
	("event1010", "type", "orbit_ev"),
	("event1010", "object", "mars"),
	("event1011", "subject", "deimos"),
	("event1011", "type", "orbit_ev"),
	("event1011", "object", "mars"),
	("event1012", "subject", "almathea"),
	("event1012", "type", "orbit_ev"),
	("event1012", "object", "jupiter"),
	("event1013", "subject", "io"),
	("event1013", "type", "orbit_ev"),
	("event1013", "object", "jupiter"),
	("event1014", "subject", "europa"),
	("event1014", "type", "orbit_ev"),
	("event1014", "object", "jupiter"),
	("event1015", "subject", "ganymede"),
	("event1015", "type", "orbit_ev"),
	("event1015", "object", "jupiter"),
	("event1016", "subject", "callisto"),
	("event1016", "type", "orbit_ev"),
	("event1016", "object", "jupiter"),
	("event1017", "subject", "jupiterthirteenth"),
	("event1017", "type", "orbit_ev"),
	("event1017", "object", "jupiter"),
	("event1018", "subject", "jupitersixth"),
	("event1018", "type", "orbit_ev"),
	("event1018", "object", "jupiter"),
	("event1019", "subject", "jupitertenth"),
	("event1019", "type", "orbit_ev"),
	("event1019", "object", "jupiter"),
	("event1020", "subject", "jupiterseventh"),
	("event1020", "type", "orbit_ev"),
	("event1020", "object", "jupiter"),
	("event1021", "subject", "jupitertwelfth"),
	("event1021", "type", "orbit_ev"),
	("event1021", "object", "jupiter"),
	("event1022", "subject", "jupitereleventh"),
	("event1022", "type", "orbit_ev"),
	("event1022", "object", "jupiter"),
	("event1023", "subject", "jupitereighth"),
	("event1023", "type", "orbit_ev"),
	("event1023", "object", "jupiter"),
	("event1024", "subject", "jupiterninth"),
	("event1024", "type", "orbit_ev"),
	("event1024", "object", "jupiter"),
	("event1025", "subject", "jupiterfourteenth"),
	("event1025", "type", "orbit_ev"),
	("event1025", "object", "jupiter"),
	("event1026", "subject", "saturnfirst"),
	("event1026", "type", "orbit_ev"),
	("event1026", "object", "saturn"),
	("event1027", "subject", "janus"),
	("event1027", "type", "orbit_ev"),
	("event1027", "object", "saturn"),
	("event1028", "subject", "mimas"),
	("event1028", "type", "orbit_ev"),
	("event1028", "object", "saturn"),
	("event1029", "subject", "enceladus"),
	("event1029", "type", "orbit_ev"),
	("event1029", "object", "saturn"),
	("event1030", "subject", "tethys"),
	("event1030", "type", "orbit_ev"),
	("event1030", "object", "saturn"),
	("event1031", "subject", "dione"),
	("event1031", "type", "orbit_ev"),
	("event1031", "object", "saturn"),
	("event1032", "subject", "rhea"),
	("event1032", "type", "orbit_ev"),
	("event1032", "object", "saturn"),
	("event1033", "subject", "titan"),
	("event1033", "type", "orbit_ev"),
	("event1033", "object", "saturn"),
	("event1034", "subject", "hyperion"),
	("event1034", "type", "orbit_ev"),
	("event1034", "object", "saturn"),
	("event1035", "subject", "iapetus"),
	("event1035", "type", "orbit_ev"),
	("event1035", "object", "saturn"),
	("event1036", "subject", "phoebe"),
	("event1036", "type", "orbit_ev"),
	("event1036", "object", "saturn"),
	("event1037", "subject", "miranda"),
	("event1037", "type", "orbit_ev"),
	("event1037", "object", "uranus"),
	("event1038", "subject", "ariel"),
	("event1038", "type", "orbit_ev"),
	("event1038", "object", "uranus"),
	("event1039", "subject", "umbriel"),
	("event1039", "type", "orbit_ev"),
	("event1039", "object", "uranus"),
	("event1040", "subject", "titania"),
	("event1040", "type", "orbit_ev"),
	("event1040", "object", "uranus"),
	("event1041", "subject", "oberon"),
	("event1041", "type", "orbit_ev"),
	("event1041", "object", "uranus"),
	("event1042", "subject", "triton"),
	("event1042", "type", "orbit_ev"),
	("event1042", "object", "neptune"),
	("event1043", "subject", "nereid"),
	("event1043", "type", "orbit_ev"),
	("event1043", "object", "neptune"),
	("event1044", "subject", "charon"),
	("event1044", "type", "orbit_ev"),
	("event1044", "object", "pluto"),

	------------- discover relation --------------
	("event1045", "subject", "hall"),
	("event1045", "type", "discover_ev"),
	("event1045", "object", "phobos"),
	("event1046", "subject", "hall"),
	("event1046", "type", "discover_ev"),
	("event1046", "object", "deimos"),
	("event1047", "subject", "bernard"),
	("event1047", "type", "discover_ev"),
	("event1047", "object", "almathea"),
	("event1048", "subject", "galileo"),
	("event1048", "type", "discover_ev"),
	("event1048", "object", "io"),
	("event1049", "subject", "galileo"),
	("event1049", "type", "discover_ev"),
	("event1049", "object", "europa"),
	("event1050", "subject", "galileo"),
	("event1050", "type", "discover_ev"),
	("event1050", "object", "ganymede"),
	("event1051", "subject", "galileo"),
	("event1051", "type", "discover_ev"),
	("event1051", "object", "callisto"),
	("event1052", "subject", "kowal"),
	("event1052", "type", "discover_ev"),
	("event1052", "object", "jupiterthirteenth"),
	("event1053", "subject", "kowal"),
	("event1053", "type", "discover_ev"),
	("event1053", "object", "jupiterfourteenth"),
	("event1054", "subject", "perrine"),
	("event1054", "type", "discover_ev"),
	("event1054", "object", "jupitersixth"),
	("event1055", "subject", "perrine"),
	("event1055", "type", "discover_ev"),
	("event1055", "object", "jupiterseventh"),
	("event1056", "subject", "nicholson"),
	("event1056", "type", "discover_ev"),
	("event1056", "object", "jupitertenth"),
	("event1057", "subject", "nicholson"),
	("event1057", "type", "discover_ev"),
	("event1057", "object", "jupitertwelfth"),
	("event1058", "subject", "nicholson"),
	("event1058", "type", "discover_ev"),
	("event1058", "object", "jupitereleventh"),
	("event1059", "subject", "nicholson"),
	("event1059", "type", "discover_ev"),
	("event1059", "object", "jupiterninth"),
	("event1060", "subject", "melotte"),
	("event1060", "type", "discover_ev"),
	("event1060", "object", "jupitereighth"),
	("event1061", "subject", "larsen"),
	("event1061", "type", "discover_ev"),
	("event1061", "object", "saturnfirst"),
	("event1062", "subject", "Fouuntain"),
	("event1062", "type", "discover_ev"),
	("event1062", "object", "saturnfirst"),
	("event1063", "subject", "dollfus"),
	("event1063", "type", "discover_ev"),
	("event1063", "object", "janus"),
	("event1064", "subject", "herschel"),
	("event1064", "type", "discover_ev"),
	("event1064", "object", "mimas"),
	("event1065", "subject", "herschel"),
	("event1065", "type", "discover_ev"),
	("event1065", "object", "enceladus"),
	("event1066", "subject", "herschel"),
	("event1066", "type", "discover_ev"),
	("event1066", "object", "titania"),
	("event1067", "subject", "herschel"),
	("event1067", "type", "discover_ev"),
	("event1067", "object", "oberon"),
	("event1068", "subject", "cassini"),
	("event1068", "type", "discover_ev"),
	("event1068", "object", "tethys"),
	("event1069", "subject", "cassini"),
	("event1069", "type", "discover_ev"),
	("event1069", "object", "dione"),
	("event1070", "subject", "cassini"),
	("event1070", "type", "discover_ev"),
	("event1070", "object", "rhea"),
	("event1071", "subject", "cassini"),
	("event1071", "type", "discover_ev"),
	("event1071", "object", "iapetus"),
	("event1072", "subject", "huygens"),
	("event1072", "type", "discover_ev"),
	("event1072", "object", "titan"),
	("event1073", "subject", "bond"),
	("event1073", "type", "discover_ev"),
	("event1073", "object", "hyperion"),
	("event1074", "subject", "pickering"),
	("event1074", "type", "discover_ev"),
	("event1074", "object", "phoebe"),
	("event1075", "subject", "kuiper"),
	("event1075", "type", "discover_ev"),
	("event1075", "object", "miranda"),
	("event1076", "subject", "kuiper"),
	("event1076", "type", "discover_ev"),
	("event1076", "object", "nereid"),
	("event1077", "subject", "lassell"),
	("event1077", "type", "discover_ev"),
	("event1077", "object", "ariel"),
	("event1078", "subject", "lassell"),
	("event1078", "type", "discover_ev"),
	("event1078", "object", "umbriel"),
	("event1079", "subject", "lassell"),
	("event1079", "type", "discover_ev"),
	("event1079", "object", "triton"),

	----------------- membership ---------------
	-- Sun --
	("event1080", "subject", "sol"),
	("event1080", "type", "membership"),
	("event1080", "object", "sun"),

	-- Planet --
	("event1081", "subject", "mercury"),
	("event1081", "type", "membership"),
	("event1081", "object", "planet"),
	("event1082", "subject", "venus"),
	("event1082", "type", "membership"),
	("event1082", "object", "planet"),
	("event1083", "subject", "earth"),
	("event1083", "type", "membership"),
	("event1083", "object", "planet"),
	("event1084", "subject", "mars"),
	("event1084", "type", "membership"),
	("event1084", "object", "planet"),
	("event1085", "subject", "jupiter"),
	("event1085", "type", "membership"),
	("event1085", "object", "planet"),
	("event1086", "subject", "saturn"),
	("event1086", "type", "membership"),
	("event1086", "object", "planet"),
	("event1087", "subject", "uranus"),
	("event1087", "type", "membership"),
	("event1087", "object", "planet"),
	("event1088", "subject", "neptune"),
	("event1088", "type", "membership"),
	("event1088", "object", "planet"),
	("event1089", "subject", "pluto"),
	("event1089", "type", "membership"),
	("event1089", "object", "planet"),

	-- Moon --
	("event1090", "subject", "luna"),
	("event1090", "type", "membership"),
	("event1090", "object", "moon"),
	("event1091", "subject", "phobos"),
	("event1091", "type", "membership"),
	("event1091", "object", "moon"),
	("event1092", "subject", "deimos"),
	("event1092", "type", "membership"),
	("event1092", "object", "moon"),
	("event1093", "subject", "almathea"),
	("event1093", "type", "membership"),
	("event1093", "object", "moon"),
	("event1094", "subject", "io"),
	("event1094", "type", "membership"),
	("event1094", "object", "moon"),
	("event1095", "subject", "europa"),
	("event1095", "type", "membership"),
	("event1095", "object", "moon"),
	("event1096", "subject", "ganymede"),
	("event1096", "type", "membership"),
	("event1096", "object", "moon"),
	("event1097", "subject", "callisto"),
	("event1097", "type", "membership"),
	("event1097", "object", "moon"),
	("event1098", "subject", "jupiterthirteenth"),
	("event1098", "type", "membership"),
	("event1098", "object", "moon"),
	("event1099", "subject", "jupitersixth"),
	("event1099", "type", "membership"),
	("event1099", "object", "moon"),
	("event1100", "subject", "jupitertenth"),
	("event1100", "type", "membership"),
	("event1100", "object", "moon"),
	("event1101", "subject", "jupiterseventh"),
	("event1101", "type", "membership"),
	("event1101", "object", "moon"),
	("event1102", "subject", "jupitertwelfth"),
	("event1102", "type", "membership"),
	("event1102", "object", "moon"),
	("event1103", "subject", "jupitereleventh"),
	("event1103", "type", "membership"),
	("event1103", "object", "moon"),
	("event1104", "subject", "jupitereighth"),
	("event1104", "type", "membership"),
	("event1104", "object", "moon"),
	("event1105", "subject", "jupiterninth"),
	("event1105", "type", "membership"),
	("event1105", "object", "moon"),
	("event1106", "subject", "jupiterfourteenth"),
	("event1106", "type", "membership"),
	("event1106", "object", "moon"),
	("event1107", "subject", "saturnfirst"),
	("event1107", "type", "membership"),
	("event1107", "object", "moon"),
	("event1108", "subject", "janus"),
	("event1108", "type", "membership"),
	("event1108", "object", "moon"),
	("event1109", "subject", "mimas"),
	("event1109", "type", "membership"),
	("event1109", "object", "moon"),
	("event1110", "subject", "enceladus"),
	("event1110", "type", "membership"),
	("event1110", "object", "moon"),
	("event1111", "subject", "tethys"),
	("event1111", "type", "membership"),
	("event1111", "object", "moon"),
	("event1112", "subject", "dione"),
	("event1112", "type", "membership"),
	("event1112", "object", "moon"),
	("event1113", "subject", "rhea"),
	("event1113", "type", "membership"),
	("event1113", "object", "moon"),
	("event1114", "subject", "titan"),
	("event1114", "type", "membership"),
	("event1114", "object", "moon"),
	("event1115", "subject", "hyperion"),
	("event1115", "type", "membership"),
	("event1115", "object", "moon"),
	("event1116", "subject", "iapetus"),
	("event1116", "type", "membership"),
	("event1116", "object", "moon"),
	("event1117", "subject", "phoebe"),
	("event1117", "type", "membership"),
	("event1117", "object", "moon"),
	("event1118", "subject", "miranda"),
	("event1118", "type", "membership"),
	("event1118", "object", "moon"),
	("event1119", "subject", "ariel"),
	("event1119", "type", "membership"),
	("event1119", "object", "moon"),
	("event1120", "subject", "umbriel"),
	("event1120", "type", "membership"),
	("event1120", "object", "moon"),
	("event1121", "subject", "titania"),
	("event1121", "type", "membership"),
	("event1121", "object", "moon"),
	("event1122", "subject", "oberon"),
	("event1122", "type", "membership"),
	("event1122", "object", "moon"),
	("event1123", "subject", "triton"),
	("event1123", "type", "membership"),
	("event1123", "object", "moon"),
	("event1124", "subject", "nereid"),
	("event1124", "type", "membership"),
	("event1124", "object", "moon"),
	("event1125", "subject", "charon"),
	("event1125", "type", "membership"),
	("event1125", "object", "moon"),

	-- People --
	("event1126", "subject", "hall"),
	("event1126", "type", "membership"),
	("event1126", "object", "person"),
	("event1127", "subject", "bernard"),
	("event1127", "type", "membership"),
	("event1127", "object", "person"),
	("event1128", "subject", "galileo"),
	("event1128", "type", "membership"),
	("event1128", "object", "person"),
	("event1129", "subject", "kowal"),
	("event1129", "type", "membership"),
	("event1129", "object", "person"),
	("event1130", "subject", "perrine"),
	("event1130", "type", "membership"),
	("event1130", "object", "person"),
	("event1131", "subject", "nicholson"),
	("event1131", "type", "membership"),
	("event1131", "object", "person"),
	("event1132", "subject", "melotte"),
	("event1132", "type", "membership"),
	("event1132", "object", "person"),
	("event1133", "subject", "larsen"),
	("event1133", "type", "membership"),
	("event1133", "object", "person"),
	("event1134", "subject", "Fouuntain"),
	("event1134", "type", "membership"),
	("event1134", "object", "person"),
	("event1135", "subject", "dollfus"),
	("event1135", "type", "membership"),
	("event1135", "object", "person"),
	("event1136", "subject", "herschel"),
	("event1136", "type", "membership"),
	("event1136", "object", "person"),
	("event1137", "subject", "cassini"),
	("event1137", "type", "membership"),
	("event1137", "object", "person"),
	("event1138", "subject", "huygens"),
	("event1138", "type", "membership"),
	("event1138", "object", "person"),
	("event1139", "subject", "bond"),
	("event1139", "type", "membership"),
	("event1139", "object", "person"),
	("event1140", "subject", "pickering"),
	("event1140", "type", "membership"),
	("event1140", "object", "person"),
	("event1141", "subject", "kuiper"),
	("event1141", "type", "membership"),
	("event1141", "object", "person"),
	("event1142", "subject", "lassell"),
	("event1142", "type", "membership"),
	("event1142", "object", "person"),
	
	--Atmospheric--
	("event1143","subject","venus"),
	("event1143","type","membership"),
	("event1143","object","atmospheric"),
	("event1144","subject","earth"),
	("event1144","type","membership"),
	("event1144","object","atmospheric"),
	("event1145","subject","io"),
	("event1145","type","membership"),
	("event1145","object","atmospheric"),
	("event1146","subject","mars"),
	("event1146","type","membership"),
	("event1146","object","atmospheric"),
	("event1147","subject","titan"),
	("event1147","type","membership"),
	("event1147","object","atmospheric"),
	("event1148","subject","venus"),
	("event1148","type","membership"),
	("event1148","object","atmospheric"),
	
	--Blue--
	("event1149","subject","earth"),
	("event1149","type","membership"),
	("event1149","object","blue"),
	("event1150","subject","neptune"),
	("event1150","type","membership"),
	("event1150","object","blue"),
	("event1151","subject","saturn"),
	("event1151","type","membership"),
	("event1151","object","blue"),
	("event1152","subject","uranus"),
	("event1152","type","membership"),
	("event1152","object","blue"),
	
	--Depressed--
	("event1153","subject","hall"),
	("event1153","type","membership"),
	("event1153","object","depressed"),
	
	--Solid--
	("event1154","subject","venus"),
	("event1154","type","membership"),
	("event1154","object","solid"),
	("event1155","subject","almathea"),
	("event1155","type","membership"),
	("event1155","object","solid"),
	("event1156","subject","ariel"),
	("event1156","type","membership"),
	("event1156","object","solid"),
	("event1157","subject","callisto"),
	("event1157","type","membership"),
	("event1157","object","solid"),
	("event1158","subject","charon"),
	("event1158","type","membership"),
	("event1158","object","solid"),
	("event1159","subject","deimos"),
	("event1159","type","membership"),
	("event1159","object","solid"),
	("event1160","subject","dione"),
	("event1160","type","membership"),
	("event1160","object","solid"),
	("event1161","subject","earth"),
	("event1161","type","membership"),
	("event1161","object","solid"),
	("event1162","subject","enceladus"),
	("event1162","type","membership"),
	("event1162","object","solid"),
	("event1163","subject","europa"),
	("event1163","type","membership"),
	("event1163","object","solid"),
	("event1164","subject","ganymede"),
	("event1164","type","membership"),
	("event1164","object","solid"),
	("event1165","subject","hyperion"),
	("event1165","type","membership"),
	("event1165","object","solid"),
	("event1166","subject","iapetus"),
	("event1166","type","membership"),
	("event1166","object","solid"),
	("event1167","subject","io"),
	("event1167","type","membership"),
	("event1167","object","solid"),
	("event1168","subject","janus"),
	("event1168","type","membership"),
	("event1168","object","solid"),
	("event1169","subject","jupiter"),
	("event1169","type","membership"),
	("event1169","object","solid"),
	("event1170","subject","jupitereighth"),
	("event1170","type","membership"),
	("event1170","object","solid"),
	("event1171","subject","jupitereleventh"),
	("event1171","type","membership"),
	("event1171","object","solid"),
	("event1172","subject","jupiterfourteenth"),
	("event1172","type","membership"),
	("event1172","object","solid"),
	("event1173","subject","jupiterninth"),
	("event1173","type","membership"),
	("event1173","object","solid"),
	("event1174","subject","jupiterseventh"),
	("event1174","type","membership"),
	("event1174","object","solid"),
	("event1175","subject","jupitersixth"),
	("event1175","type","membership"),
	("event1175","object","solid"),
	("event1176","subject","jupitertenth"),
	("event1176","type","membership"),
	("event1176","object","solid"),
	("event1177","subject","jupiterthirteenth"),
	("event1177","type","membership"),
	("event1177","object","solid"),
	("event1178","subject","jupitertwelfth"),
	("event1178","type","membership"),
	("event1178","object","solid"),
	("event1179","subject","luna"),
	("event1179","type","membership"),
	("event1179","object","solid"),
	("event1180","subject","mars"),
	("event1180","type","membership"),
	("event1180","object","solid"),
	("event1181","subject","mercury"),
	("event1181","type","membership"),
	("event1181","object","solid"),
	("event1182","subject","mimas"),
	("event1182","type","membership"),
	("event1182","object","solid"),
	("event1183","subject","miranda"),
	("event1183","type","membership"),
	("event1183","object","solid"),
	("event1184","subject","neptune"),
	("event1184","type","membership"),
	("event1184","object","solid"),
	("event1185","subject","nereid"),
	("event1185","type","membership"),
	("event1185","object","solid"),
	("event1186","subject","oberon"),
	("event1186","type","membership"),
	("event1186","object","solid"),
	("event1187","subject","phobos"),
	("event1187","type","membership"),
	("event1187","object","solid"),
	("event1188","subject","phoebe"),
	("event1188","type","membership"),
	("event1188","object","solid"),
	("event1189","subject","pluto"),
	("event1189","type","membership"),
	("event1189","object","solid"),
	("event1190","subject","rhea"),
	("event1190","type","membership"),
	("event1190","object","solid"),
	("event1191","subject","saturn"),
	("event1191","type","membership"),
	("event1191","object","solid"),
	("event1192","subject","saturnfirst"),
	("event1192","type","membership"),
	("event1192","object","solid"),
	("event1193","subject","tethys"),
	("event1193","type","membership"),
	("event1193","object","solid"),
	("event1194","subject","titan"),
	("event1194","type","membership"),
	("event1194","object","solid"),
	("event1195","subject","titania"),
	("event1195","type","membership"),
	("event1195","object","solid"),
	("event1196","subject","triton"),
	("event1196","type","membership"),
	("event1196","object","solid"),
	("event1197","subject","umbriel"),
	("event1197","type","membership"),
	("event1197","object","solid"),
	("event1198","subject","uranus"),
	("event1198","type","membership"),
	("event1198","object","solid"),
	("event1199","subject","venus"),
	("event1199","type","membership"),
	("event1199","object","solid"),
	
	--Brown--
	("event1200","subject","venus"),
	("event1200","type","membership"),
	("event1200","object","brown"),
	("event1201","subject","mercury"),
	("event1201","type","membership"),
	("event1201","object","brown"),
	("event1202","subject","pluto"),
	("event1202","type","membership"),
	("event1202","object","brown"),
	("event1203","subject","venus"),
	("event1203","type","membership"),
	("event1203","object","brown"),
	
	--Gaseous--
	("event1204","subject","jupiter"),
	("event1204","type","membership"),
	("event1204","object","gaseous"),
	("event1205","subject","neptune"),
	("event1205","type","membership"),
	("event1205","object","gaseous"),
	("event1206","subject","saturn"),
	("event1206","type","membership"),
	("event1206","object","gaseous"),
	("event1207","subject","uranus"),
	("event1207","type","membership"),
	("event1207","object","gaseous"),
	
	--Green--
	("event1208","subject","earth"),
	("event1208","type","membership"),
	("event1208","object","green"),
	("event1209","subject","neptune"),
	("event1209","type","membership"),
	("event1209","object","green"),
	("event1210","subject","uranus"),
	("event1210","type","membership"),
	("event1210","object","green"),
	
	--Red--
	("event1211","subject","io"),
	("event1211","type","membership"),
	("event1211","object","red"),
	("event1212","subject","jupiter"),
	("event1212","type","membership"),
	("event1212","object","red"),
	("event1213","subject","mars"),
	("event1213","type","membership"),
	("event1213","object","red"),
	("event1214","subject","saturn"),
	("event1214","type","membership"),
	("event1214","object","red"),
	
	--Ringed--
	("event1215","subject","jupiter"),
	("event1215","type","membership"),
	("event1215","object","ringed"),
	("event1216","subject","neptune"),
	("event1216","type","membership"),
	("event1216","object","ringed"),
	("event1217","subject","saturn"),
	("event1217","type","membership"),
	("event1217","object","ringed"),
	("event1218","subject","uranus"),
	("event1218","type","membership"),
	("event1218","object","ringed"),
	
	--Vacuumous--
	("event1219","subject","venus"),
	("event1219","type","membership"),
	("event1219","object","vacuumous"),
	("event1220","subject","almathea"),
	("event1220","type","membership"),
	("event1220","object","vacuumous"),
	("event1221","subject","ariel"),
	("event1221","type","membership"),
	("event1221","object","vacuumous"),
	("event1222","subject","callisto"),
	("event1222","type","membership"),
	("event1222","object","vacuumous"),
	("event1223","subject","charon"),
	("event1223","type","membership"),
	("event1223","object","vacuumous"),
	("event1224","subject","deimos"),
	("event1224","type","membership"),
	("event1224","object","vacuumous"),
	("event1225","subject","dione"),
	("event1225","type","membership"),
	("event1225","object","vacuumous"),
	("event1226","subject","earth"),
	("event1226","type","membership"),
	("event1226","object","vacuumous"),
	("event1227","subject","enceladus"),
	("event1227","type","membership"),
	("event1227","object","vacuumous"),
	("event1228","subject","europa"),
	("event1228","type","membership"),
	("event1228","object","vacuumous"),
	("event1229","subject","ganymede"),
	("event1229","type","membership"),
	("event1229","object","vacuumous"),
	("event1230","subject","hyperion"),
	("event1230","type","membership"),
	("event1230","object","vacuumous"),
	("event1231","subject","iapetus"),
	("event1231","type","membership"),
	("event1231","object","vacuumous"),
	("event1232","subject","io"),
	("event1232","type","membership"),
	("event1232","object","vacuumous"),
	("event1233","subject","janus"),
	("event1233","type","membership"),
	("event1233","object","vacuumous"),
	("event1234","subject","jupiter"),
	("event1234","type","membership"),
	("event1234","object","vacuumous"),
	("event1235","subject","jupitereighth"),
	("event1235","type","membership"),
	("event1235","object","vacuumous"),
	("event1236","subject","jupitereleventh"),
	("event1236","type","membership"),
	("event1236","object","vacuumous"),
	("event1237","subject","jupiterfourteenth"),
	("event1237","type","membership"),
	("event1237","object","vacuumous"),
	("event1238","subject","jupiterninth"),
	("event1238","type","membership"),
	("event1238","object","vacuumous"),
	("event1239","subject","jupiterseventh"),
	("event1239","type","membership"),
	("event1239","object","vacuumous"),
	("event1240","subject","jupitersixth"),
	("event1240","type","membership"),
	("event1240","object","vacuumous"),
	("event1241","subject","jupitertenth"),
	("event1241","type","membership"),
	("event1241","object","vacuumous"),
	("event1242","subject","jupiterthirteenth"),
	("event1242","type","membership"),
	("event1242","object","vacuumous"),
	("event1243","subject","jupitertwelfth"),
	("event1243","type","membership"),
	("event1243","object","vacuumous"),
	("event1244","subject","luna"),
	("event1244","type","membership"),
	("event1244","object","vacuumous"),
	("event1245","subject","mars"),
	("event1245","type","membership"),
	("event1245","object","vacuumous"),
	("event1246","subject","mercury"),
	("event1246","type","membership"),
	("event1246","object","vacuumous"),
	("event1247","subject","mimas"),
	("event1247","type","membership"),
	("event1247","object","vacuumous"),
	("event1248","subject","miranda"),
	("event1248","type","membership"),
	("event1248","object","vacuumous"),
	("event1249","subject","neptune"),
	("event1249","type","membership"),
	("event1249","object","vacuumous"),
	("event1250","subject","nereid"),
	("event1250","type","membership"),
	("event1250","object","vacuumous"),
	("event1251","subject","oberon"),
	("event1251","type","membership"),
	("event1251","object","vacuumous"),
	("event1252","subject","phobos"),
	("event1252","type","membership"),
	("event1252","object","vacuumous"),
	("event1253","subject","phoebe"),
	("event1253","type","membership"),
	("event1253","object","vacuumous"),
	("event1254","subject","pluto"),
	("event1254","type","membership"),
	("event1254","object","vacuumous"),
	("event1255","subject","rhea"),
	("event1255","type","membership"),
	("event1255","object","vacuumous"),
	("event1256","subject","saturn"),
	("event1256","type","membership"),
	("event1256","object","vacuumous"),
	("event1257","subject","saturnfirst"),
	("event1257","type","membership"),
	("event1257","object","vacuumous"),
	("event1258","subject","tethys"),
	("event1258","type","membership"),
	("event1258","object","vacuumous"),
	("event1259","subject","titan"),
	("event1259","type","membership"),
	("event1259","object","vacuumous"),
	("event1260","subject","titania"),
	("event1260","type","membership"),
	("event1260","object","vacuumous"),
	("event1261","subject","triton"),
	("event1261","type","membership"),
	("event1261","object","vacuumous"),
	("event1262","subject","umbriel"),
	("event1262","type","membership"),
	("event1262","object","vacuumous"),
	("event1263","subject","uranus"),
	("event1263","type","membership"),
	("event1263","object","vacuumous"),
	
	--Thing--
	("event1264","subject","bernard"),
	("event1264","type","membership"),
	("event1264","object","thing"),
	("event1265","subject","bond"),
	("event1265","type","membership"),
	("event1265","object","thing"),
	("event1266","subject","venus"),
	("event1266","type","membership"),
	("event1266","object","thing"),
	("event1267","subject","cassini"),
	("event1267","type","membership"),
	("event1267","object","thing"),
	("event1268","subject","dollfus"),
	("event1268","type","membership"),
	("event1268","object","thing"),
	("event1269","subject","Fouuntain"),
	("event1269","type","membership"),
	("event1269","object","thing"),
	("event1270","subject","galileo"),
	("event1270","type","membership"),
	("event1270","object","thing"),
	("event1271","subject","hall"),
	("event1271","type","membership"),
	("event1271","object","thing"),
	("event1272","subject","herschel"),
	("event1272","type","membership"),
	("event1272","object","thing"),
	("event1273","subject","huygens"),
	("event1273","type","membership"),
	("event1273","object","thing"),
	("event1274","subject","kowal"),
	("event1274","type","membership"),
	("event1274","object","thing"),
	("event1275","subject","kuiper"),
	("event1275","type","membership"),
	("event1275","object","thing"),
	("event1276","subject","larsen"),
	("event1276","type","membership"),
	("event1276","object","thing"),
	("event1277","subject","lassell"),
	("event1277","type","membership"),
	("event1277","object","thing"),
	("event1278","subject","melotte"),
	("event1278","type","membership"),
	("event1278","object","thing"),
	("event1279","subject","nicholson"),
	("event1279","type","membership"),
	("event1279","object","thing"),
	("event1280","subject","perrine"),
	("event1280","type","membership"),
	("event1280","object","thing"),
	("event1281","subject","pickering"),
	("event1281","type","membership"),
	("event1281","object","thing"),
	("event1282","subject","almathea"),
	("event1282","type","membership"),
	("event1282","object","thing"),
	("event1283","subject","ariel"),
	("event1283","type","membership"),
	("event1283","object","thing"),
	("event1284","subject","callisto"),
	("event1284","type","membership"),
	("event1284","object","thing"),
	("event1285","subject","charon"),
	("event1285","type","membership"),
	("event1285","object","thing"),
	("event1286","subject","deimos"),
	("event1286","type","membership"),
	("event1286","object","thing"),
	("event1287","subject","dione"),
	("event1287","type","membership"),
	("event1287","object","thing"),
	("event1288","subject","earth"),
	("event1288","type","membership"),
	("event1288","object","thing"),
	("event1289","subject","enceladus"),
	("event1289","type","membership"),
	("event1289","object","thing"),
	("event1290","subject","europa"),
	("event1290","type","membership"),
	("event1290","object","thing"),
	("event1291","subject","ganymede"),
	("event1291","type","membership"),
	("event1291","object","thing"),
	("event1292","subject","hyperion"),
	("event1292","type","membership"),
	("event1292","object","thing"),
	("event1293","subject","iapetus"),
	("event1293","type","membership"),
	("event1293","object","thing"),
	("event1294","subject","io"),
	("event1294","type","membership"),
	("event1294","object","thing"),
	("event1295","subject","janus"),
	("event1295","type","membership"),
	("event1295","object","thing"),
	("event1296","subject","jupiter"),
	("event1296","type","membership"),
	("event1296","object","thing"),
	("event1297","subject","jupitereighth"),
	("event1297","type","membership"),
	("event1297","object","thing"),
	("event1298","subject","jupitereleventh"),
	("event1298","type","membership"),
	("event1298","object","thing"),
	("event1299","subject","jupiterfourteenth"),
	("event1299","type","membership"),
	("event1299","object","thing"),
	("event1300","subject","jupiterninth"),
	("event1300","type","membership"),
	("event1300","object","thing"),
	("event1301","subject","jupiterseventh"),
	("event1301","type","membership"),
	("event1301","object","thing"),
	("event1302","subject","jupitersixth"),
	("event1302","type","membership"),
	("event1302","object","thing"),
	("event1303","subject","jupitertenth"),
	("event1303","type","membership"),
	("event1303","object","thing"),
	("event1304","subject","jupiterthirteenth"),
	("event1304","type","membership"),
	("event1304","object","thing"),
	("event1305","subject","jupitertwelfth"),
	("event1305","type","membership"),
	("event1305","object","thing"),
	("event1306","subject","luna"),
	("event1306","type","membership"),
	("event1306","object","thing"),
	("event1307","subject","mars"),
	("event1307","type","membership"),
	("event1307","object","thing"),
	("event1308","subject","mercury"),
	("event1308","type","membership"),
	("event1308","object","thing"),
	("event1309","subject","mimas"),
	("event1309","type","membership"),
	("event1309","object","thing"),
	("event1310","subject","miranda"),
	("event1310","type","membership"),
	("event1310","object","thing"),
	("event1311","subject","neptune"),
	("event1311","type","membership"),
	("event1311","object","thing"),
	("event1312","subject","nereid"),
	("event1312","type","membership"),
	("event1312","object","thing"),
	("event1313","subject","oberon"),
	("event1313","type","membership"),
	("event1313","object","thing"),
	("event1314","subject","phobos"),
	("event1314","type","membership"),
	("event1314","object","thing"),
	("event1315","subject","phoebe"),
	("event1315","type","membership"),
	("event1315","object","thing"),
	("event1316","subject","pluto"),
	("event1316","type","membership"),
	("event1316","object","thing"),
	("event1317","subject","rhea"),
	("event1317","type","membership"),
	("event1317","object","thing"),
	("event1318","subject","saturn"),
	("event1318","type","membership"),
	("event1318","object","thing"),
	("event1319","subject","saturnfirst"),
	("event1319","type","membership"),
	("event1319","object","thing"),
	("event1320","subject","sol"),
	("event1320","type","membership"),
	("event1320","object","thing"),
	("event1321","subject","tethys"),
	("event1321","type","membership"),
	("event1321","object","thing"),
	("event1322","subject","titan"),
	("event1322","type","membership"),
	("event1322","object","thing"),
	("event1323","subject","titania"),
	("event1323","type","membership"),
	("event1323","object","thing"),
	("event1324","subject","triton"),
	("event1324","type","membership"),
	("event1324","object","thing"),
	("event1325","subject","umbriel"),
	("event1325","type","membership"),
	("event1325","object","thing"),
	("event1326","subject","uranus"),
	("event1326","type","membership"),
	("event1326","object","thing"),
	
	--Spin--
	("event1327","subject","venus"),
	("event1327","type","membership"),
	("event1327","object","spin"),
	("event1328","subject","almathea"),
	("event1328","type","membership"),
	("event1328","object","spin"),
	("event1329","subject","ariel"),
	("event1329","type","membership"),
	("event1329","object","spin"),
	("event1330","subject","callisto"),
	("event1330","type","membership"),
	("event1330","object","spin"),
	("event1331","subject","charon"),
	("event1331","type","membership"),
	("event1331","object","spin"),
	("event1332","subject","deimos"),
	("event1332","type","membership"),
	("event1332","object","spin"),
	("event1333","subject","dione"),
	("event1333","type","membership"),
	("event1333","object","spin"),
	("event1334","subject","earth"),
	("event1334","type","membership"),
	("event1334","object","spin"),
	("event1335","subject","enceladus"),
	("event1335","type","membership"),
	("event1335","object","spin"),
	("event1336","subject","europa"),
	("event1336","type","membership"),
	("event1336","object","spin"),
	("event1337","subject","ganymede"),
	("event1337","type","membership"),
	("event1337","object","spin"),
	("event1338","subject","hyperion"),
	("event1338","type","membership"),
	("event1338","object","spin"),
	("event1339","subject","iapetus"),
	("event1339","type","membership"),
	("event1339","object","spin"),
	("event1340","subject","io"),
	("event1340","type","membership"),
	("event1340","object","spin"),
	("event1341","subject","janus"),
	("event1341","type","membership"),
	("event1341","object","spin"),
	("event1342","subject","jupiter"),
	("event1342","type","membership"),
	("event1342","object","spin"),
	("event1343","subject","jupitereighth"),
	("event1343","type","membership"),
	("event1343","object","spin"),
	("event1344","subject","jupitereleventh"),
	("event1344","type","membership"),
	("event1344","object","spin"),
	("event1345","subject","jupiterfourteenth"),
	("event1345","type","membership"),
	("event1345","object","spin"),
	("event1346","subject","jupiterninth"),
	("event1346","type","membership"),
	("event1346","object","spin"),
	("event1347","subject","jupiterseventh"),
	("event1347","type","membership"),
	("event1347","object","spin"),
	("event1348","subject","jupitersixth"),
	("event1348","type","membership"),
	("event1348","object","spin"),
	("event1349","subject","jupitertenth"),
	("event1349","type","membership"),
	("event1349","object","spin"),
	("event1350","subject","jupiterthirteenth"),
	("event1350","type","membership"),
	("event1350","object","spin"),
	("event1351","subject","jupitertwelfth"),
	("event1351","type","membership"),
	("event1351","object","spin"),
	("event1352","subject","luna"),
	("event1352","type","membership"),
	("event1352","object","spin"),
	("event1353","subject","mars"),
	("event1353","type","membership"),
	("event1353","object","spin"),
	("event1354","subject","mercury"),
	("event1354","type","membership"),
	("event1354","object","spin"),
	("event1355","subject","mimas"),
	("event1355","type","membership"),
	("event1355","object","spin"),
	("event1356","subject","miranda"),
	("event1356","type","membership"),
	("event1356","object","spin"),
	("event1357","subject","neptune"),
	("event1357","type","membership"),
	("event1357","object","spin"),
	("event1358","subject","nereid"),
	("event1358","type","membership"),
	("event1358","object","spin"),
	("event1359","subject","oberon"),
	("event1359","type","membership"),
	("event1359","object","spin"),
	("event1360","subject","phobos"),
	("event1360","type","membership"),
	("event1360","object","spin"),
	("event1361","subject","phoebe"),
	("event1361","type","membership"),
	("event1361","object","spin"),
	("event1362","subject","pluto"),
	("event1362","type","membership"),
	("event1362","object","spin"),
	("event1363","subject","rhea"),
	("event1363","type","membership"),
	("event1363","object","spin"),
	("event1364","subject","saturn"),
	("event1364","type","membership"),
	("event1364","object","spin"),
	("event1365","subject","saturnfirst"),
	("event1365","type","membership"),
	("event1365","object","spin"),
	("event1366","subject","sol"),
	("event1366","type","membership"),
	("event1366","object","spin"),
	("event1367","subject","tethys"),
	("event1367","type","membership"),
	("event1367","object","spin"),
	("event1368","subject","titan"),
	("event1368","type","membership"),
	("event1368","object","spin"),
	("event1369","subject","titania"),
	("event1369","type","membership"),
	("event1369","object","spin"),
	("event1370","subject","triton"),
	("event1370","type","membership"),
	("event1370","object","spin"),
	("event1371","subject","umbriel"),
	("event1371","type","membership"),
	("event1371","object","spin"),
	("event1372","subject","uranus"),
	("event1372","type","membership"),
	("event1372","object","spin")
	
	]
	
--copied from gangster_v4: utility functions for making lists unique
subset s t = (s \\ t) == []

makeset x = Set.toList $ Set.fromList x
	
--copied from gangster_v4: combinators
termor tmph1 tmph2 = f
    where
	f setofevs =
		(tmph1 setofevs) || (tmph2 setofevs)

termand tmph1 tmph2 = f
    where
	f setofevs =
		(tmph1 setofevs) && (tmph2 setofevs)

that = nounand

nounand s t = intersect s t

nounor s t = makeset(s ++ t)
                       
a nph vbph =
	length (intersect  nph vbph) /= 0 

every nph vbph =
	subset  nph vbph 

no nph vbph =
	length (intersect nph vbph) == 0
	
none nph vbph =
	no nph vbph
  
one nph vbph =
	length (intersect  nph vbph) == 1 
  

two nph vbph =
	length (intersect  nph vbph) == 2 
  

which nph vbph = if result /= [] then result else "none."
	where result = unwords $ intersect nph vbph

how_many nph vbph =
	show $ length (intersect nph vbph)

who vbph = which person vbph

--New
what = which thing

--end of copied from gangster_v4

--namespace_gts = "http://richard.myweb.cs.uwindsor.ca/ESWC/solarman_triplestore#"
namespace_gts = "" --Don't need URIs yet

gts fragment = namespace_gts ++ fragment

sun		= get_members dataStore (gts "sun")
planet	= get_members dataStore (gts "planet")
moon	= get_members dataStore (gts "moon")
person	= get_members dataStore (gts "person")
thing	= get_members dataStore (gts "thing")

atmospheric = get_members dataStore (gts "atmospheric")
blue = get_members dataStore (gts "blue")
depressed = get_members dataStore (gts "depressed")
solid = get_members dataStore (gts "solid")
brown 	= get_members dataStore (gts "brown")
gaseous = get_members dataStore (gts "gaseous")
green 	= get_members dataStore (gts "green")
red		= get_members dataStore (gts "red")
ringed  = get_members dataStore (gts "ringed")
vacuumous = get_members dataStore (gts "vacuumous")
exists = thing
spin   = get_members dataStore (gts "spin")


--Need to ensure uniqueness in output?  I.e. discover_intrans has multiple "hall"s in list
discover_intrans	= get_subjs_of_event_type dataStore (gts "discover_ev")
orbit_intrans		= get_subjs_of_event_type dataStore (gts "orbit_ev")

discover = make_relation dataStore "discover_ev" 
discovered = discover

orbit = make_relation dataStore "orbit_ev" 
orbited = orbit

hall = elem $ gts "hall"
phobos = elem $ gts "phobos"
mars = elem $ gts "mars"

{-Renamed from make_trans_image_from_intrans, does not cache anymore.
Reason:
1) with the triple store backend interface being separated from the implementation,
it is now necessary to pass the store into any function that would use it.
Since image_discover/orbit uses dataStore, if make_relation were to use, say a different store by accident,
(it must be passed in alongside the cached image), it's possible the results from two different stores
could be mixed together.  By having make_relation handle that itself, that programming error will never happen.
2) Haskell is lazily evaluated and GHC does a lot of caching in the background anyway.  It'd likely be done automatically
-}

--Need two, because one needs to be inverted for a grammar rule to work.  
--Could just flip arguments, but this is more readable
make_relation :: (TripleStore m) => m -> String -> ([String] -> Bool) -> [String]
make_relation ev_data rel tmph
	= [subj | (subj, evs) <- make_image ev_data rel "subject",
				tmph (concat [getts_3 ev_data (ev, gts "object", "?") | ev <- evs])]
				
make_inverted_relation :: (TripleStore m) => m -> String -> ([String] -> Bool) -> [String]
make_inverted_relation ev_data rel tmph
	= [obj | (obj, evs) <- make_image ev_data rel "object",
				tmph (concat [getts_3 ev_data (ev, gts "subject", "?") | ev <- evs])]
				
--Prepositional filtering (not used yet)

filter_ev :: (TripleStore m) => m -> Event -> [(String, [String] -> Bool)] -> Bool
filter_ev ev_data ev [] = True
filter_ev ev_data ev (prep:list_of_preps)
	= ((snd (prep)) (getts_3 ev_data (ev,fst (prep),"?"))) && filter_ev ev_data ev list_of_preps
	
--New from Eric
--make_filtered_image tmph preps image =
--	[subj | (subj, evs) <- image,
--				tmph (concat [getts_3 ev_data (ev, gts "object", "?") | ev <- evs, filter_ev ev preps])]
			   
--The prime version of these functions denote filtered versions based on a set of prepositions
--discover' tmph preps = make_filtered_image tmph preps discover_image
--orbit' tmph preps = make_filtered_image tmph preps orbit_image

--TODO:
--Move combinators and relation stuff into modules?

--Copied from old solarman:
yesno x = if x then "yes." else "no"
sand True True = True
sand any any'  = False


{-
||-----------------------------------------------------------------------------
||  BASIC INTERPRETERS
||-----------------------------------------------------------------------------
-}


pnoun           =  pre_processed Pnoun
cnoun           =  pre_processed Cnoun
adj             =  pre_processed Adj
det             =  pre_processed Det
intransvb       =  pre_processed Intransvb
transvb         =  pre_processed Transvb
linkingvb       =  pre_processed Linkingvb
relpron         =  pre_processed Relpron
termphjoin      =  pre_processed Termphjoin
verbphjoin      =  pre_processed Verbphjoin
nounjoin        =  pre_processed Nounjoin
prep            =  pre_processed Prep
indefpron       =  pre_processed Indefpron
{-
terminator      =  uninterpreted (SPECIAL_SYMBOL_TERM ".")
                   $orelse
                   uninterpreted (SPECIAL_SYMBOL_TERM "?")
                   $orelse
                   uninterpreted (SPECIAL_SYMBOL_TERM "\n")
-}
sentjoin        =  pre_processed Sentjoin
quest1          =  pre_processed Quest1
quest2          =  pre_processed Quest2
quest3          =  pre_processed Quest3
quest4a         =  pre_processed Quest4a
quest4b         =  pre_processed Quest4b

pre_processed key 
 = let formAlts altTerminals  = memoize key (altTerminals) 
       formTerminal [x]       = x
       formTerminal (x:xs)    = x <|>  formTerminal xs
       list_of_ters           = [ terminal (term a) z 
                                | (a,b,z) <- dictionary
                                , b == key]
   in  formAlts (formTerminal list_of_ters)


meaning_of p dInp key
 = let dInput     = words dInp
       appParser  = unState (p T0 [] ((1,[]), dInput) ([],[])) [] 
       upperBound = (length dInput) + 1
   in  formFinal key upperBound (snd $ appParser)   

meaning_of_ p dInp key
 = let dInput     = words dInp
       appParser  = unState (p T0 [] ((1,[]), dInput) ([],[])) [] 
       upperBound = (length dInput) + 1
   in  (snd $ appParser)   

formAtts key ePoint t 
 = concat $ concat $ concat $ concat  
   [[[[  val1 |(id1,val1)<-synAtts]
   	       |(((st,inAtt2),(end,synAtts)), ts)<-rs, st == 1 && end == ePoint]     
   	        |((i,inAt1),((cs,ct),rs)) <- sr ]
   	         |(s,sr) <- t, s == key ]
formFinal key ePoint t 
 = concat $ concat $ concat $ concat  
   [[[[  val1 |(id1,val1)<-synAtts]
   	       |(((st,inAtt2),(end,synAtts)), ts)<-rs, st == 1 && end == ePoint]     
   	        |((i,inAt1),((cs,ct),rs)) <- sr ]
   	         |(s,sr) <- t, s == key ]  	         
{-
test p = unState (p ((1,[]),input) ([],[])) [] 

main   = do putStr  $ render80 $ formatAtts Question $ snd $ test (question T0 [])

type Start1   = (Int, InsAttVals)
type Start    = ((Int,InsAttVals), [String])
type End      = (Int, InsAttVals)
type Atts     = [AttValue] -- [(AttType, AttValue)]
type InsAttVals = [(Instance, Atts)]


type Mtable   = [(MemoL
                 ,[(Start1,(Context,Result))]
                 )
                ] 
type Result   = [((Start1, End),[Tree MemoL])]
||-----------------------------------------------------------------------------
|| THE ATTRIBUTE GRAMMAR
||-----------------------------------------------------------------------------
-}

snouncla 
 = memoize Snouncla
 (parser
  (nt cnoun S3) 
  [rule_s NOUNCLA_VAL OF LHS ISEQUALTO copy [synthesized NOUNCLA_VAL OF S3]]
  <|>
  parser (nt adjs S1  *> nt cnoun S2)
  [rule_s NOUNCLA_VAL OF LHS ISEQUALTO intrsct1 [synthesized ADJ_VAL      OF  S1,
                                                 synthesized NOUNCLA_VAL  OF  S2]]
                                               
 )

-------------------------------------------------------------------------------
relnouncla   
 = memoize Relnouncla
   (parser 
    (nt snouncla S1  *> nt relpron S2  *> nt joinvbph S3)
    [rule_s NOUNCLA_VAL OF LHS ISEQUALTO apply_middle1[synthesized NOUNCLA_VAL  OF S1,
                                                       synthesized RELPRON_VAL  OF S2,
                                                       synthesized VERBPH_VAL   OF S3]]
    <|>   
    parser
    (nt snouncla S4)
    [rule_s NOUNCLA_VAL OF LHS ISEQUALTO copy [synthesized NOUNCLA_VAL OF S4]]
   )
----------------------------------------------------------------------------



nouncla 
 = memoize Nouncla 
   (parser (nt relnouncla S1 *> nt nounjoin S2 *> nt nouncla S3)
    [rule_s NOUNCLA_VAL OF LHS ISEQUALTO apply_middle2 [synthesized NOUNCLA_VAL  OF S1,
                                                        synthesized NOUNJOIN_VAL OF S2,
                                                        synthesized NOUNCLA_VAL  OF S3]]
    <|>
    parser (nt relnouncla S1 *> nt relpron S2 *> nt linkingvb S3 *> nt nouncla S4)
    [rule_s NOUNCLA_VAL  OF LHS ISEQUALTO apply_middle3 [synthesized NOUNCLA_VAL  OF S1,
                                                         synthesized RELPRON_VAL  OF S2,
                                                         synthesized NOUNCLA_VAL  OF S4]]
    <|>
    parser (nt relnouncla S1)
    [rule_s NOUNCLA_VAL  OF LHS ISEQUALTO copy [synthesized NOUNCLA_VAL  OF S1]]
   )

------------------------------------------------------------------------------
adjs   
 = memoize Adjs      
   (parser (nt adj S1 *> nt adjs S2)
    [rule_s ADJ_VAL  OF LHS ISEQUALTO intrsct2 [synthesized ADJ_VAL  OF S1,
                                                synthesized ADJ_VAL  OF S2]]
    <|>
    parser (nt adj S3)
    [rule_s ADJ_VAL  OF LHS ISEQUALTO copy [synthesized ADJ_VAL  OF S3]]
   )
------------------------------------------------------------------------------

detph     
 = memoize Detph
   (parser (nt indefpron S3)
    [rule_s TERMPH_VAL OF LHS ISEQUALTO copy [synthesized TERMPH_VAL OF S3]]
    <|>
    parser (nt det S1 *> nt nouncla S2)
    [rule_s TERMPH_VAL OF LHS ISEQUALTO applydet [synthesized DET_VAL      OF S1,
                                                  synthesized NOUNCLA_VAL  OF S2]]
   )                                              

----------------------------------------------------------------------------------
transvbph 
 = memoize Transvbph
   (parser (nt transvb S1 *> nt jointermph S2)
    [rule_s VERBPH_VAL OF LHS ISEQUALTO applytransvb [synthesized VERB_VAL    OF S1,
                                                      synthesized TERMPH_VAL  OF S2]]
    <|>
    parser (nt linkingvb S1 *> nt transvb S2 *> nt prep S3 *> nt jointermph S4)
    [rule_s VERBPH_VAL  OF LHS ISEQUALTO drop3rd [synthesized LINKINGVB_VAL  OF  S1,
                                                  synthesized VERB_VAL       OF  S2,
                                                  synthesized PREP_VAL       OF  S3,
                                                  synthesized TERMPH_VAL     OF  S4]]
   )

-------------------------------------------------------------------------------

verbph 
 = memoize Verbph
   (
    parser (nt transvbph S4)
    [rule_s VERBPH_VAL OF LHS ISEQUALTO copy [synthesized VERBPH_VAL OF S4]]
   <|>
    parser (nt intransvb S5)
    [rule_s VERBPH_VAL OF LHS ISEQUALTO copy [synthesized VERBPH_VAL OF S5]]
   <|>
    parser (nt linkingvb S1 *> nt det S2 *> nt nouncla S3)
    [rule_s VERBPH_VAL OF LHS ISEQUALTO applyvbph [synthesized NOUNCLA_VAL OF S3]]
   )
------------------------------------------------------------------------------------

termph    
 = memoize Termph  
   (
   parser (nt pnoun S1) 
   [rule_s TERMPH_VAL OF LHS ISEQUALTO copy [synthesized TERMPH_VAL OF S1]]
   <|>  
   parser (nt detph S2)
   [rule_s TERMPH_VAL OF LHS ISEQUALTO copy [synthesized TERMPH_VAL OF S2]]
   )
             

------------------------------------------------------------------------------------
jointermph 
 = memoize Jointermph 
   (
{-    parser (nt jointermph S1 *> nt termphjoin S2 *> nt termph S3)
        [rule_s TERMPH_VAL  OF LHS ISEQUALTO appjoin1 [synthesized TERMPH_VAL     OF S1,
                                                       synthesized TERMPHJOIN_VAL OF S2,
                                                       synthesized TERMPH_VAL     OF S3]]
   <|>
-}
    parser (nt jointermph S1 *> nt termphjoin S2 *> nt jointermph S3)
    [rule_s TERMPH_VAL  OF LHS ISEQUALTO appjoin1 [synthesized TERMPH_VAL     OF S1,
                                                   synthesized TERMPHJOIN_VAL OF S2,
                                                   synthesized TERMPH_VAL     OF S3]]
   <|>
    parser (nt termph S4)
    [rule_s TERMPH_VAL  OF LHS ISEQUALTO copy [synthesized TERMPH_VAL  OF S4]]
   )
------------------------------------------------------------------------------------
joinvbph  
 = memoize Joinvbph   
   (
   parser (nt verbph S1  *> nt verbphjoin S2  *> nt joinvbph S3)
   [rule_s VERBPH_VAL  OF LHS ISEQUALTO appjoin2 [synthesized VERBPH_VAL    OF S1,
                                                  synthesized VBPHJOIN_VAL  OF S2,
                                                  synthesized VERBPH_VAL    OF S3]]
    <|>
    parser (nt verbph S4)
    [rule_s VERBPH_VAL  OF LHS ISEQUALTO copy [synthesized VERBPH_VAL  OF S4]]
   )
---------------------------------------------------------------------------
sent  
 = memoize Sent
   (
    parser (nt jointermph S1  *> nt joinvbph S2)
    [rule_s SENT_VAL OF LHS ISEQUALTO apply_termphrase [synthesized TERMPH_VAL  OF  S1,
                                                        synthesized VERBPH_VAL  OF  S2]]
   )
-- **************************************************************************** --
two_sent 
 = memoize Two_sent
   (
    parser (nt sent S1 *> nt sentjoin S2 *> nt sent S3)
    [rule_s SENT_VAL OF LHS ISEQUALTO sent_val_comp [synthesized SENT_VAL      OF  S1,
                                                     synthesized SENTJOIN_VAL  OF  S2,
                                                     synthesized SENT_VAL      OF  S3]] 
   )
------------------------------------------------------------------------------------

question   
 = memoize Question  
   (
    parser (nt quest1 S1  *> nt sent S2 )
    [rule_s QUEST_VAL  OF LHS ISEQUALTO ans1 [synthesized QUEST1_VAL  OF  S1,
                                              synthesized SENT_VAL    OF  S2]]  
    <|>
    parser (nt quest2 S1 *> nt joinvbph S2)
    [rule_s QUEST_VAL  OF LHS ISEQUALTO ans2 [synthesized QUEST2_VAL    OF  S1,
                                              synthesized VERBPH_VAL    OF  S2]] 
    <|>
    parser (nt quest3 S1 *> nt nouncla S2 *> nt joinvbph S3)
    [rule_s QUEST_VAL  OF LHS ISEQUALTO ans3 [synthesized QUEST3_VAL  OF S1,
                                              synthesized NOUNCLA_VAL OF S2,
                                              synthesized VERBPH_VAL  OF  S3]]
    <|>
    parser (nt quest4 S1 *> nt nouncla S2 *> nt joinvbph S3)
    [rule_s QUEST_VAL  OF LHS ISEQUALTO ans3 [synthesized QUEST3_VAL  OF  S1,
                                              synthesized NOUNCLA_VAL OF  S2,
                                              synthesized VERBPH_VAL  OF  S3]]
    <|>
    parser (nt two_sent S1)
    [rule_s QUEST_VAL OF LHS ISEQUALTO truefalse [synthesized SENT_VAL OF  S1]]  
    <|>
    parser (nt sent S1)
    [rule_s QUEST_VAL  OF LHS ISEQUALTO truefalse [synthesized SENT_VAL OF  S1]]

   )
quest4 = memoize Quest4 
   (
   parser (nt quest4a S1 *> nt quest4b S2) 
   [rule_s QUEST3_VAL  OF LHS ISEQUALTO copy [synthesized QUEST3_VAL  OF  S1]] 
   )
---------------------------------------------------------------------------------

query = memoize Query
        (
        parser (nt question S1) -- *> nt terminator S2)
        [rule_s QUEST_VAL  OF LHS ISEQUALTO copy [synthesized QUEST_VAL  OF  S1]]
        )



{-
|| -----------------------------------------------------------------------------
|| THE SEMANTICS - PART I : The attribute evaluation  functions
||-----------------------------------------------------------------------------
applyBiOp [e1,op,e2] 
                  = \atts -> VAL ((getAtts getB_OP atts op ) (getAtts getAVAL atts e1 ) (getAtts getAVAL atts e2))

-}
-- getAtts f (y,i) x = f (head (x y i))
-- copy      [b]     = \(atts,i) -> head (b atts i)

intrsct1         [x, y]    
 = \atts -> NOUNCLA_VAL (intersect (getAtts getAVALS atts x) (getAtts getAVALS atts y))

intrsct2         [x, y]         
 = \atts -> ADJ_VAL (intersect (getAtts getAVALS atts x) (getAtts getAVALS atts y))

applydet         [x, y]                 
 = \atts -> TERMPH_VAL ((getAtts getDVAL atts x) (getAtts getAVALS atts y) )
 
--make_trans_vb is very similar to make_relation.  getBR must mean "get binary relation"
--getTVAL must mean "get predicate" i.e. what would be "phobos" in "discover phobos"
--Changed to get rid of make_trans_vb, since the getBR attribute was changed to not 
--be a binary relation but instead a function that make_relation would give
--I.e., the kind of function that make_trans_vb would have generated, since they were
--nearly identical
applytransvb     [x, y]     
 -- = \atts -> VERBPH_VAL ((make_trans_vb (getAtts getBR atts x)) (getAtts getTVAL atts y))
 = \atts -> VERBPH_VAL (make_relation dataStore (getAtts getBR atts x) (getAtts getTVAL atts y))

applyvbph        [z]    
 = \atts -> VERBPH_VAL (getAtts getAVALS atts z)
 
appjoin1         [x, y, z]     
 = \atts -> TERMPH_VAL ((getAtts getTJVAL atts y) (getAtts getTVAL atts x) (getAtts getTVAL atts z))

appjoin2         [x, y, z]    
 = \atts -> VERBPH_VAL ((getAtts getVJVAL atts y) (getAtts getAVALS atts x) (getAtts getAVALS atts z))

apply_middle1    [x, y, z]    
 = \atts -> NOUNCLA_VAL ((getAtts getRELVAL atts y) (getAtts getAVALS atts x) (getAtts getAVALS atts z))

apply_middle2    [x, y, z]                  
 = \atts -> NOUNCLA_VAL ((getAtts getNJVAL atts y) (getAtts getAVALS atts x) (getAtts getAVALS atts z))

apply_middle3    [x, y, z]    
 = \atts -> NOUNCLA_VAL ((getAtts getRELVAL atts y) (getAtts getAVALS atts x) (getAtts getAVALS atts z))

--DONE
drop3rd          [w, x, y, z]     
-- = \atts -> VERBPH_VAL ((make_trans_vb (invert (getAtts getBR atts x))) (getAtts getTVAL atts z))
-- Think "a orbited by b" vs "b orbits a"
	= \atts -> VERBPH_VAL (make_inverted_relation dataStore (getAtts getBR atts x) (getAtts getTVAL atts z))

apply_termphrase [x, y]     
 = \atts -> SENT_VAL ((getAtts getTVAL atts x) (getAtts getAVALS atts y) )
 
sent_val_comp    [s1, f, s2]      
 = \atts -> SENT_VAL ((getAtts getSJVAL atts f) (getAtts getSV atts s1) (getAtts getSV atts s2))

ans1             [x, y]       
 = \atts -> QUEST_VAL ((getAtts getQU1VAL atts x) (getAtts getSV atts y) )

ans2             [x, y]     
 = \atts -> QUEST_VAL ((getAtts getQU2VAL atts x) (getAtts getAVALS atts y))

ans3             [x, y, z]     
 = \atts -> QUEST_VAL ((getAtts getQU3VAL atts x) (getAtts getAVALS atts y) (getAtts getAVALS atts z))

truefalse        [x]       
 = \atts -> if (getAtts getSV atts x) then (QUEST_VAL "true.") else (QUEST_VAL "false.")


{-
||-----------------------------------------------------------------------------
|| THE SEMANTICS - PART II : Functions used to obtain objects denoted by 
||   proper nouns, verbs, etc.
||-----------------------------------------------------------------------------

|| FUNCTION USED TO DEFINE OBJECTS ASSOCIATED WITH PROPER NOUNS
-}
test_wrt e s = e `elem` s

-- FUNCTION USED TO DEFINE MEANINGS OF VERBS IN TERMS OF RELATIONS
make_trans_vb rel p = [x | (x, image_x) <- collect rel, p image_x] -- Similar to make_relation

dictionary = 
 [("thing",              Cnoun,     [NOUNCLA_VAL thing]),
  ("things",             Cnoun,     [NOUNCLA_VAL thing]),
  ("planets",            Cnoun,     [NOUNCLA_VAL planet]),
  ("planet",             Cnoun,     [NOUNCLA_VAL planet]),
  ("person",              Cnoun,    [NOUNCLA_VAL person]),   
  ("sun",                Cnoun,     [NOUNCLA_VAL sun]), 
  ("moon",               Cnoun,     [NOUNCLA_VAL moon]), 
  ("moons",              Cnoun,     [NOUNCLA_VAL moon]),
  ("satellite",          Cnoun,     [NOUNCLA_VAL moon]),
  ("satellites",         Cnoun,     [NOUNCLA_VAL moon]),
  --DONE
  --should replace with set constructions as defined below by get_members
  ("atmospheric",        Adj,       [ADJ_VAL     atmospheric]),
  ("blue",               Adj,       [ADJ_VAL     blue]),
  ("blue",               Adj,       [ADJ_VAL     depressed]),
  ("solid",              Adj,       [ADJ_VAL     solid]),     
  ("brown",              Adj,       [ADJ_VAL     brown]),   
  ("gaseous",            Adj,       [ADJ_VAL     gaseous]),  
  ("green",              Adj,       [ADJ_VAL     green]),  
  ("red",                Adj,       [ADJ_VAL     red]),  
  ("ringed",             Adj,       [ADJ_VAL     ringed]),  
  ("vacuumous",          Adj,       [ADJ_VAL     vacuumous]),
  ("exist",              Intransvb, [VERBPH_VAL  thing]),
  ("exists",             Intransvb, [VERBPH_VAL  thing]),
  ("spin",               Intransvb, [VERBPH_VAL  spin]),
  ("spins",              Intransvb, [VERBPH_VAL  spin]),
  --TODO: DONE
  --replace sets of things with a gts call to (gts "?" "property" "spin/thing/ringed/red/blue/etc")
  ("the",                Det,       [DET_VAL a]),
  ("a",                  Det,       [DET_VAL a]),
  ("one",                Det,       [DET_VAL one]), 
  ("an",                 Det,       [DET_VAL a]), 
  ("some",               Det,       [DET_VAL a]), 
  ("any",                Det,       [DET_VAL a]), 
  ("no",                 Det,       [DET_VAL no]), 
  ("every",              Det,       [DET_VAL every]), 
  ("all",                Det,       [DET_VAL every]),  
  ("two",                Det,       [DET_VAL two]),
  {-DONE
	function_denoted_by_a xs ys       = length( intersect xs ys ) > 0
	function_denoted_by_every xs ys   = includes xs ys
	function_denoted_by_none xs ys    = length( intersect xs ys ) == 0
	function_denoted_by_one xs ys     = length( intersect xs ys ) == 1
	function_denoted_by_two xs ys     = length( intersect xs ys ) == 2
  -}
  ("bernard",            Pnoun,     [TERMPH_VAL (List.elem $ gts "bernard")]),
  ("bond",               Pnoun,     [TERMPH_VAL (List.elem $ gts "bond")]),
  ("venus",              Pnoun,     [TERMPH_VAL (List.elem $ gts "venus")]),
  ("cassini",            Pnoun,     [TERMPH_VAL (List.elem $ gts "cassini")]),
  ("dollfus",            Pnoun,     [TERMPH_VAL (List.elem $ gts "dollfus")]),
  ("Fouuntain",          Pnoun,     [TERMPH_VAL (List.elem $ gts "Fouuntain")]),
  ("galileo",            Pnoun,     [TERMPH_VAL (List.elem $ gts "galileo")]),
  ("hall",               Pnoun,     [TERMPH_VAL (List.elem $ gts "hall")]),
  ("herschel",           Pnoun,     [TERMPH_VAL (List.elem $ gts "herschel")]),
  ("huygens",            Pnoun,     [TERMPH_VAL (List.elem $ gts "huygens")]),
  ("kowal",              Pnoun,     [TERMPH_VAL (List.elem $ gts "kowal")]),
  ("kuiper",             Pnoun,     [TERMPH_VAL (List.elem $ gts "kuiper")]),
  ("larsen",             Pnoun,     [TERMPH_VAL (List.elem $ gts "larsen")]),
  ("lassell",            Pnoun,     [TERMPH_VAL (List.elem $ gts "lassell")]),
  ("melotte",            Pnoun,     [TERMPH_VAL (List.elem $ gts "melotte")]),
  ("nicholson",          Pnoun,     [TERMPH_VAL (List.elem $ gts "nicholson")]),
  ("perrine",            Pnoun,     [TERMPH_VAL (List.elem $ gts "perrine")]),
  ("pickering",          Pnoun,     [TERMPH_VAL (List.elem $ gts "pickering")]),
  ("almathea",           Pnoun,     [TERMPH_VAL (List.elem $ gts "almathea")]),
  ("ariel",              Pnoun,     [TERMPH_VAL (List.elem $ gts "ariel")]),
  ("callisto",           Pnoun,     [TERMPH_VAL (List.elem $ gts "callisto")]),
  ("charon",             Pnoun,     [TERMPH_VAL (List.elem $ gts "charon")]),
  ("deimos",             Pnoun,     [TERMPH_VAL (List.elem $ gts "deimos")]),
  ("dione",              Pnoun,     [TERMPH_VAL (List.elem $ gts "dione")]),
  ("earth",              Pnoun,     [TERMPH_VAL (List.elem $ gts "earth")]),
  ("enceladus",          Pnoun,     [TERMPH_VAL (List.elem $ gts "enceladus")]),
  ("europa",             Pnoun,     [TERMPH_VAL (List.elem $ gts "europa")]),
  ("ganymede",           Pnoun,     [TERMPH_VAL (List.elem $ gts "ganymede")]),
  ("hyperion",           Pnoun,     [TERMPH_VAL (List.elem $ gts "hyperion")]),
  ("iapetus",            Pnoun,     [TERMPH_VAL (List.elem $ gts "iapetus")]),
  ("io",                 Pnoun,     [TERMPH_VAL (List.elem $ gts "io")]),
  ("janus",              Pnoun,     [TERMPH_VAL (List.elem $ gts "janus")]),
  ("jupiter",            Pnoun,     [TERMPH_VAL (List.elem $ gts "jupiter")]),
  ("jupitereighth",      Pnoun,     [TERMPH_VAL (List.elem $ gts "jupitereighth")]),
  ("jupitereleventh",    Pnoun,     [TERMPH_VAL (List.elem $ gts "jupitereleventh")]),
  ("jupiterfourteenth",  Pnoun,     [TERMPH_VAL (List.elem $ gts "jupiterfourteenth")]),
  ("jupiterninth",       Pnoun,     [TERMPH_VAL (List.elem $ gts "jupiterninth")]),
  ("jupiterseventh",     Pnoun,     [TERMPH_VAL (List.elem $ gts "jupiterseventh")]),
  ("jupitersixth",       Pnoun,     [TERMPH_VAL (List.elem $ gts "jupitersixth")]),
  ("jupitertenth",       Pnoun,     [TERMPH_VAL (List.elem $ gts "jupitertenth")]),
  ("jupiterthirteenth",  Pnoun,     [TERMPH_VAL (List.elem $ gts "jupiterthirteenth")]),
  ("jupitertwelfth",     Pnoun,     [TERMPH_VAL (List.elem $ gts "jupitertwelfth")]),
  ("luna",               Pnoun,     [TERMPH_VAL (List.elem $ gts "luna")]),
  ("mars",               Pnoun,     [TERMPH_VAL (List.elem $ gts "mars")]),
  ("mercury",            Pnoun,     [TERMPH_VAL (List.elem $ gts "mercury")]),
  ("mimas",              Pnoun,     [TERMPH_VAL (List.elem $ gts "mimas")]),
  ("miranda",            Pnoun,     [TERMPH_VAL (List.elem $ gts "miranda")]),
  ("neptune",            Pnoun,     [TERMPH_VAL (List.elem $ gts "neptune")]),
  ("nereid",             Pnoun,     [TERMPH_VAL (List.elem $ gts "nereid")]),
  ("oberon",             Pnoun,     [TERMPH_VAL (List.elem $ gts "oberon")]),
  ("phobos",             Pnoun,     [TERMPH_VAL (List.elem $ gts "phobos")]),
  ("phoebe",             Pnoun,     [TERMPH_VAL (List.elem $ gts "phoebe")]),
  ("pluto",              Pnoun,     [TERMPH_VAL (List.elem $ gts "pluto")]),
  ("rhea",               Pnoun,     [TERMPH_VAL (List.elem $ gts "rhea")]),
  ("saturn",             Pnoun,     [TERMPH_VAL (List.elem $ gts "saturn")]),
  ("saturnfirst",        Pnoun,     [TERMPH_VAL (List.elem $ gts "saturnfirst")]),
  ("sol",                Pnoun,     [TERMPH_VAL (List.elem $ gts "sol")]),
  ("tethys",             Pnoun,     [TERMPH_VAL (List.elem $ gts "tethys")]),
  ("titan",              Pnoun,     [TERMPH_VAL (List.elem $ gts "titan")]),
  ("titania",            Pnoun,     [TERMPH_VAL (List.elem $ gts "titania")]),
  ("triton",             Pnoun,     [TERMPH_VAL (List.elem $ gts "triton")]),
  ("umbriel",            Pnoun,     [TERMPH_VAL (List.elem $ gts "umbriel")]),
  ("uranus",             Pnoun,     [TERMPH_VAL (List.elem $ gts "uranus")]),
  ("venus",              Pnoun,     [TERMPH_VAL (List.elem $ gts "venus")]),
  --these should be converted to [TERMPH_VAL (List.elem $ gts "subject")] DONE
  ("discover",           Transvb,   [VERB_VAL ("discover_ev")]),
  ("discovers",          Transvb,   [VERB_VAL ("discover_ev")]),
  ("discovered",         Transvb,   [VERB_VAL ("discover_ev")]),
  ("orbit",              Transvb,   [VERB_VAL ("orbit_ev")]),
  ("orbited",            Transvb,   [VERB_VAL ("orbit_ev")]),
  ("orbits",             Transvb,   [VERB_VAL ("orbit_ev")]),
  --Instead of rel_orbit or rel_discover (DONE)
  --we should link to the function denoted by the verb, or maybe get every event by type "discover"/"steal"
  ("is",                 Linkingvb, [LINKINGVB_VAL  id]),
  ("was",                Linkingvb, [LINKINGVB_VAL  id]), 
  ("are",                Linkingvb, [LINKINGVB_VAL  id]),
  ("were",               Linkingvb, [LINKINGVB_VAL  id]),
  ("that",               Relpron,   [RELPRON_VAL    that]),
  ("who",                Relpron,   [RELPRON_VAL    that]),
  ("which",              Relpron,   [RELPRON_VAL    that]),
  ("and",                Verbphjoin,[VBPHJOIN_VAL   nounand]),
  ("or",                 Verbphjoin,[VBPHJOIN_VAL   nounor]),
  ("and",                Nounjoin,  [NOUNJOIN_VAL   nounand]),
  ("or",                 Nounjoin,  [NOUNJOIN_VAL   nounor]),
  ("by",                 Prep,      [PREP_VAL       id]),
  ("and",                Termphjoin,[TERMPHJOIN_VAL termand]),
  ("or",                 Termphjoin,[TERMPHJOIN_VAL termor]), 
  ("and",                Sentjoin,  [SENTJOIN_VAL   sand]), --TODO checked.  sand was fine
  ("does",               Quest1,    [QUEST1_VAL     yesno]),
  ("did",                Quest1  ,  [QUEST1_VAL     yesno]),
  ("do",                 Quest1,    [QUEST1_VAL     yesno]),
  ("what",               Quest2,    [QUEST2_VAL     what]), --TODO: definition for what
  ("who",                Quest2,    [QUEST2_VAL     who]), --Correct, according to orignal definition
  ("which",              Quest3,    [QUEST3_VAL     which]),
  ("what",               Quest3,    [QUEST3_VAL     which]),
  ("how",                Quest4a,   [QUEST3_VAL     how_many]),
  ("many",               Quest4b,   [QUEST3_VAL     how_many])]
  ++
  [("human",       Cnoun,    meaning_of nouncla "person" Nouncla),
   ("discoverer",  Cnoun,    meaning_of nouncla 
                               "person who discovered something" Nouncla),
   ("discoverers", Cnoun,    meaning_of nouncla 
                               "person who discovered something" Nouncla), 
   ("humans",      Cnoun,    meaning_of nouncla "person" Nouncla), 
   ("people",      Cnoun,    meaning_of nouncla "person" Nouncla),
   ("orbit",       Intransvb,meaning_of verbph  "orbit something" Verbph),
   ("orbits",      Intransvb,meaning_of verbph  "orbit something" Verbph),
   ("anyone",      Indefpron,meaning_of detph   "a person" Detph),
   ("anything",    Indefpron,meaning_of detph   "a thing" Detph),
   ("anybody",     Indefpron,meaning_of detph   "a person" Detph),
   ("someone",     Indefpron,meaning_of detph   "a person" Detph),
   ("something",   Indefpron,meaning_of detph   "a thing" Detph),
   ("somebody",    Indefpron,meaning_of detph   "a person" Detph),
   ("everyone",    Indefpron,meaning_of detph   "every person" Detph),
   ("everything",  Indefpron,meaning_of detph   "every thing" Detph),
   ("everybody",   Indefpron,meaning_of detph   "every person" Detph),
   ("nobody",      Indefpron,meaning_of detph   "no person" Detph),
   ("noone",       Indefpron,meaning_of detph   "no person" Detph)]


test1 p p_ inp = do putStr  $ render80 $ format{-Atts p_-} $ snd $ unState (p T0 [] ((1,[]),words inp) ([],[])) [] 
test p input = unState (p ((1,[]),input) ([],[])) [] 



main  i = formatAttsFinalAlt Question  ((length (words i))+1) $ snd $ test (question T0 []) (words i)


findStart st ((s,ss):rest) | s == st   = [(s,ss)]
                           | otherwise = findStart st rest
findStart st []                        = []     

input = words i1

i1 = "which moons that were discovered by hall orbit mars" -- OK
i2 = "who discovered a moon that orbits mars" -- OK
i3 = "did hall discover every moon that orbits mars" -- OK
i4 = "how many moons were discovered by hall and kuiper" -- OK
i5 = "how many moons were discovered by hall or kuiper" -- OK
i6 = "every moon was discovered by a person" -- OK
i7 = "which planets are orbited by a moon that was discovered by galileo" -- OK
i8 = "which moons were discovered by nobody" -- OK
i9 = "is every planet orbited by a moon" -- Broken in original too
i10 = "which planets are orbited by two moons" -- OK
i11 = "who was the discoverer of phobos" -- Broken in original too
i12 = "hall discovered a moon that orbits mars" -- OK
i13 = "which moons that orbit mars were discovered by hall" -- OK
i14 = "every moon orbits every planet" -- OK
i15 = "every planet is orbited by a moon" -- OK
i16 = "a planet is orbited by a moon" -- OK
i17 = "does phobos orbit mars" -- OK
--
i18 = "did hall discover deimos or phobos and miranda" -- Broken in original too
i19 = "did hall discover deimos or phobos and miranda or deimos and deimos" -- Broken in original too
