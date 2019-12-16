
-- ----------------------------
-- 混袋相关
-- ----------------------------
INSERT INTO  `auth_role_resource`(`role_id`, `resource_id`, `order_num`, `deleted`, `create_user`, `create_time`, `update_time`) VALUES (1, 190, NULL, 0, 1001, '2019-11-25 11:35:05', '2019-11-25 11:35:07');
INSERT INTO  `auth_role_resource`(`role_id`, `resource_id`, `order_num`, `deleted`, `create_user`, `create_time`, `update_time`) VALUES (10, 190, NULL, 0, 1001, '2019-11-25 11:35:16', '2019-11-25 11:35:18');

-- ----------------------------
-- Table structure for auth_resource
-- ----------------------------
DROP TABLE IF EXISTS `auth_resource`;
CREATE TABLE `auth_resource`  (
  `id` int(10) NOT NULL AUTO_INCREMENT COMMENT '菜单id',
  `pid` int(10) DEFAULT 0 COMMENT '父id',
  `sub_sys` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NOT NULL COMMENT '子系统标识',
  `name` varchar(70) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NOT NULL COMMENT '菜单名称',
  `resource_type` varchar(60) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '菜单类型',
  `icon` varchar(200) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '菜单图标',
  `path` varchar(200) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '菜单url',
  `component` varchar(200) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '菜单com破net',
  `order_num` int(11) DEFAULT NULL COMMENT '菜单序号',
  `is_hide` tinyint(1) DEFAULT NULL COMMENT '是否隐藏0展开1隐藏',
  `position` varchar(60) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '菜单所在位置',
  `show_type` tinyint(1) DEFAULT NULL,
  `deleted` tinyint(1) NOT NULL DEFAULT 0 COMMENT '是否删除0未删除1已删除',
  `create_time` datetime(0) NOT NULL COMMENT '创建时间',
  `update_time` timestamp(0) DEFAULT NULL ON UPDATE CURRENT_TIMESTAMP(0) COMMENT '修改时间',
  PRIMARY KEY (`id`) USING BTREE,
  UNIQUE INDEX `index_position`(`position`) USING BTREE,
  INDEX `idx_name`(`name`) USING BTREE
) ENGINE = InnoDB AUTO_INCREMENT = 2675 CHARACTER SET = utf8mb4 COLLATE = utf8mb4_0900_ai_ci COMMENT = '资源表' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of auth_resource
-- ----------------------------
INSERT INTO `auth_resource` VALUES (1, 0, 'cloud_maf_app', '采购人员', 'function', '', 'purchaser', NULL, NULL, 1, '1', NULL, 0, '2019-03-15 00:00:00', '2019-10-24 17:59:29');
INSERT INTO `auth_resource` VALUES (10, 0, 'cloud_maf_app', '物流人员', 'function', NULL, 'logistics', NULL, NULL, 1, '2', NULL, 0, '2019-07-19 08:54:07', '2019-10-10 11:28:12');
INSERT INTO `auth_resource` VALUES (20, 0, 'cloud_maf_app', '取样人员', 'function', NULL, 'samplePerson', NULL, NULL, 1, '3', NULL, 0, '2019-09-25 17:40:07', '2019-10-10 11:28:13');
INSERT INTO `auth_resource` VALUES (30, 0, 'cloud_maf_app', '化验人员', 'function', NULL, 'assayPerson', NULL, NULL, 1, '4', NULL, 0, '2019-09-25 09:51:49', '2019-10-10 11:28:14');
INSERT INTO `auth_resource` VALUES (40, 0, 'cloud_maf_app', '物流承运商', 'function', NULL, 'transLogistics', NULL, NULL, 1, '5', NULL, 0, '2019-09-25 17:38:22', '2019-10-10 11:28:16');
INSERT INTO `auth_resource` VALUES (50, 0, 'cloud_maf_app', '磅房人员', 'function', NULL, 'weighingPerson', NULL, NULL, 1, '6', NULL, 0, '2019-09-25 17:39:22', '2019-10-10 11:28:17');
INSERT INTO `auth_resource` VALUES (60, 0, 'cloud_maf_app', '仓储人员', 'function', NULL, 'warehousePerson', NULL, NULL, 1, '7', NULL, 0, '2019-10-09 15:14:13', '2019-10-10 11:28:18');
INSERT INTO `auth_resource` VALUES (70, 0, 'cloud_maf_app', '质检主管', 'function', NULL, 'detectionManager', NULL, NULL, 1, '8', NULL, 0, '2019-09-30 11:01:59', '2019-10-10 11:28:20');
INSERT INTO `auth_resource` VALUES (80, 0, 'cloud_maf_app', '工艺主管', 'function', NULL, 'crafts', NULL, NULL, 1, '9', NULL, 0, '2019-10-09 15:16:51', '2019-10-10 11:28:27');
INSERT INTO `auth_resource` VALUES (90, 0, 'cloud_maf_app', '配料人员', 'function', NULL, 'batching', NULL, NULL, 1, '10', NULL, 0, '2019-09-30 11:04:54', '2019-10-10 11:28:29');
INSERT INTO `auth_resource` VALUES (100, 0, 'cloud_maf_app', '矿热炉记录', 'function', NULL, 'RKEF', NULL, NULL, 1, '11', NULL, 0, '2019-09-30 11:06:53', '2019-10-10 11:28:30');
INSERT INTO `auth_resource` VALUES (110, 0, 'cloud_maf_app', '成品装袋', 'function', NULL, 'bag', NULL, NULL, 1, '12', NULL, 0, '2019-09-30 11:09:21', '2019-10-10 11:28:31');
INSERT INTO `auth_resource` VALUES (120, 0, 'cloud_maf_app', '设备巡检', 'function', NULL, 'inspect', NULL, NULL, 1, '13', NULL, 0, '2019-09-30 11:10:53', '2019-10-10 11:28:33');
INSERT INTO `auth_resource` VALUES (130, 0, 'cloud_maf_app', '车次确认', 'function', NULL, 'trainGoods', NULL, NULL, 1, '14', NULL, 0, '2019-10-09 15:18:32', '2019-10-10 11:28:35');
INSERT INTO `auth_resource` VALUES (140, 0, 'cloud_maf_app', '供销结算', 'function', NULL, 'settlement', NULL, NULL, 1, '15', NULL, 1, '2019-10-10 11:23:45', '2019-11-25 10:14:37');
INSERT INTO `auth_resource` VALUES (150, 0, 'cloud_maf_app', '销售人员', 'function', NULL, 'saleman', NULL, NULL, 1, '16', NULL, 0, '2019-10-10 11:24:16', '2019-10-10 11:28:37');
INSERT INTO `auth_resource` VALUES (160, 0, 'cloud_maf_app', '二维码查询', 'function', NULL, 'codeQuery', NULL, NULL, 1, '17', NULL, 0, '2019-11-11 11:30:39', '2019-11-25 11:33:52');
INSERT INTO `auth_resource` VALUES (190, 0, 'cloud_maf_app', '混袋', 'function', NULL, 'mixPackge', NULL, NULL, 1, '20', NULL, 0, '2019-11-25 11:29:34', '2019-11-25 11:33:49');
INSERT INTO `auth_resource` VALUES (200, 0, 'cloud_maf_app', '关于蜜云', 'default', NULL, 'about', NULL, NULL, 1, '21', NULL, 0, '2019-11-25 17:20:32', '2019-11-27 16:11:07');
INSERT INTO `auth_resource` VALUES (1000, 0, 'cloud_maf_web', '设备管理', 'function', 'setting', 'device', NULL, NULL, 1, '100', NULL, 0, '2019-03-15 00:00:00', '2019-10-22 11:23:37');
INSERT INTO `auth_resource` VALUES (1010, 1000, 'cloud_maf_web', '电表管理', 'page', NULL, 'ammeter', NULL, NULL, 0, '100-1', NULL, 0, '2019-07-19 08:54:07', '2019-10-22 11:45:32');
INSERT INTO `auth_resource` VALUES (1020, 1000, 'cloud_maf_web', '电价管理', 'page', NULL, 'electricityPrice', NULL, NULL, 0, '100-2', NULL, 0, '2019-07-19 08:54:07', '2019-10-22 11:43:19');
INSERT INTO `auth_resource` VALUES (1030, 1000, 'cloud_maf_web', '设备巡检', 'page', NULL, 'inspection', NULL, NULL, 0, '100-3', NULL, 0, '2019-07-19 08:54:07', '2019-10-22 11:43:21');
INSERT INTO `auth_resource` VALUES (1040, 1000, 'cloud_maf_web', '设备管理', 'page', NULL, 'deviceManage', NULL, NULL, 0, '100-4', NULL, 0, '2019-07-19 08:54:07', '2019-10-22 11:43:34');
INSERT INTO `auth_resource` VALUES (1050, 1000, 'cloud_maf_web', '称重设备管理', 'page', NULL, 'weighDevice', NULL, NULL, 0, '100-5', NULL, 0, '2019-09-24 16:04:36', '2019-10-22 11:43:39');
INSERT INTO `auth_resource` VALUES (1060, 1000, 'cloud_maf_web', 'PLC设备管理', 'page', NULL, 'plcManage', NULL, NULL, 0, '100-6', NULL, 0, '2019-09-25 09:51:49', '2019-10-22 11:43:43');
INSERT INTO `auth_resource` VALUES (1070, 1000, 'cloud_maf_web', '网关管理', 'page', NULL, 'gateway', NULL, NULL, 0, '100-7', NULL, 0, '2019-09-25 09:53:32', '2019-10-22 11:43:45');
INSERT INTO `auth_resource` VALUES (1100, 0, 'cloud_maf_web', '产品管理', 'function', 'bars', 'product', NULL, NULL, 1, '110', NULL, 0, '2019-09-25 17:37:37', '2019-10-22 11:43:51');
INSERT INTO `auth_resource` VALUES (1110, 1100, 'cloud_maf_web', '产品档案', 'page', NULL, 'archives', NULL, NULL, 0, '110-1', NULL, 0, '2019-09-25 17:38:22', '2019-10-22 13:10:34');
INSERT INTO `auth_resource` VALUES (1120, 1100, 'cloud_maf_web', '产品类别', 'page', NULL, 'productCategory', NULL, NULL, 0, '110-2', NULL, 0, '2019-09-25 09:53:32', '2019-10-22 13:10:37');
INSERT INTO `auth_resource` VALUES (1200, 0, 'cloud_maf_web', '质检管理', 'function', 'ci', 'quality', NULL, NULL, 1, '120', NULL, 0, '2019-09-25 17:37:37', '2019-10-22 07:12:06');
INSERT INTO `auth_resource` VALUES (1210, 1200, 'cloud_maf_web', '化验属性', 'page', NULL, 'labProp', NULL, NULL, 0, '120-1', NULL, 0, '2019-09-25 17:39:22', '2019-10-22 07:12:09');
INSERT INTO `auth_resource` VALUES (1300, 0, 'cloud_maf_web', '物流管理', 'function', 'diff', 'logistical', NULL, NULL, 1, '130', NULL, 0, '2019-09-25 09:53:32', '2019-10-22 07:12:12');
INSERT INTO `auth_resource` VALUES (1310, 1300, 'cloud_maf_web', '地点配置', 'page', NULL, 'locationConfig', NULL, NULL, 0, '130-1', NULL, 0, '2019-09-25 17:37:37', '2019-10-22 11:44:26');
INSERT INTO `auth_resource` VALUES (1400, 0, 'cloud_maf_web', '仓库管理', 'function', 'apartment', 'warehouse', NULL, NULL, 1, '140', NULL, 0, '2019-09-25 17:40:07', '2019-10-22 07:12:15');
INSERT INTO `auth_resource` VALUES (1410, 1400, 'cloud_maf_web', '仓库档案', 'page', NULL, 'warehouseFile', NULL, NULL, 0, '140-1', NULL, 0, '2019-09-25 09:53:32', '2019-10-22 11:44:33');
INSERT INTO `auth_resource` VALUES (1420, 1400, 'cloud_maf_web', '期初库存', 'page', NULL, 'stock', NULL, NULL, 0, '140-2', NULL, 0, '2019-09-25 17:37:37', '2019-10-22 11:44:37');
INSERT INTO `auth_resource` VALUES (1500, 0, 'cloud_maf_web', '统计配置', 'function', 'reconciliation', 'statistics', NULL, NULL, 1, '150', NULL, 0, '2019-03-15 00:00:00', '2019-10-22 07:12:18');
INSERT INTO `auth_resource` VALUES (1510, 1500, 'cloud_maf_web', '原料吨耗配置', 'page', NULL, 'materialCons', NULL, NULL, 0, '150-1', NULL, 0, '2019-10-22 11:27:31', '2019-10-22 11:45:33');
INSERT INTO `auth_resource` VALUES (1520, 1500, 'cloud_maf_web', '原料损耗配置', 'page', NULL, 'lossConfig', NULL, NULL, 0, '150-2', NULL, 0, '2019-10-22 11:35:03', '2019-10-22 11:45:34');
INSERT INTO `auth_resource` VALUES (1530, 1500, 'cloud_maf_web', '看板BI配置', 'page', NULL, 'boardConfig', NULL, NULL, 0, '150-3', NULL, 0, '2019-10-22 11:35:23', '2019-10-22 11:45:35');
INSERT INTO `auth_resource` VALUES (1540, 1500, 'cloud_maf_web', '报表配置', 'page', NULL, 'reportConfig', NULL, NULL, 0, '150-4', NULL, 0, '2019-10-22 11:35:47', '2019-10-22 11:45:35');
INSERT INTO `auth_resource` VALUES (1600, 0, 'cloud_maf_web', '客户及供应商管理', 'function', 'usergroup-add', 'crm', NULL, NULL, 1, '160', NULL, 0, '2019-10-22 11:36:46', '2019-10-22 07:12:21');
INSERT INTO `auth_resource` VALUES (1610, 1600, 'cloud_maf_web', '客户管理', 'page', NULL, 'clientManage', NULL, NULL, 0, '160-1', NULL, 0, '2019-10-22 11:37:09', '2019-10-22 11:45:37');
INSERT INTO `auth_resource` VALUES (1620, 1600, 'cloud_maf_web', '供应商管理', 'page', NULL, 'supplierManage', NULL, NULL, 0, '160-2', NULL, 0, '2019-10-22 11:37:27', '2019-10-22 11:45:38');
INSERT INTO `auth_resource` VALUES (1630, 1600, 'cloud_maf_web', '上下游账户管理', 'page', NULL, 'accountManage', NULL, NULL, 0, '160-3', NULL, 0, '2019-10-22 11:37:47', '2019-10-22 11:45:39');
INSERT INTO `auth_resource` VALUES (1700, 0, 'cloud_maf_web', '权限配置', 'function', 'block', 'permission', NULL, NULL, 1, '170', NULL, 0, '2019-10-22 11:38:40', '2019-10-22 07:12:24');
INSERT INTO `auth_resource` VALUES (1710, 1700, 'cloud_maf_web', '人员管理', 'page', NULL, 'userManage', NULL, NULL, 0, '170-1', NULL, 0, '2019-10-22 11:38:56', '2019-10-22 11:45:40');
INSERT INTO `auth_resource` VALUES (1720, 1700, 'cloud_maf_web', '角色管理', 'page', NULL, 'roleManage', NULL, NULL, 0, '170-2', NULL, 0, '2019-10-22 11:39:15', '2019-10-22 13:07:09');
INSERT INTO `auth_resource` VALUES (2000, 0, 'cloud_maf_bi', '总览', 'function', NULL, '/board', NULL, NULL, 1, '200', NULL, 0, '2019-10-31 18:19:57', '2019-10-31 18:25:26');
INSERT INTO `auth_resource` VALUES (2100, 0, 'cloud_maf_bi', '报表', 'menu', NULL, '/reportForm', NULL, NULL, 0, '210', NULL, 0, '2019-10-31 18:20:46', '2019-11-08 03:13:27');
INSERT INTO `auth_resource` VALUES (2101, 2100, 'cloud_maf_bi', '库存报表', 'function', NULL, '/reportForm/inventory', NULL, NULL, 1, '210-3', NULL, 0, '2019-10-31 18:20:46', '2019-10-31 18:36:56');
INSERT INTO `auth_resource` VALUES (2102, 2100, 'cloud_maf_bi', '质检报表', 'function', NULL, '/reportForm/qualityInspection', NULL, NULL, 1, '210-4', NULL, 0, '2019-10-31 18:20:46', '2019-10-31 18:36:59');
INSERT INTO `auth_resource` VALUES (2103, 2100, 'cloud_maf_bi', '采购报表', 'function', NULL, '/reportForm/purchase', NULL, NULL, 1, '210-5', NULL, 0, '2019-10-31 18:20:46', '2019-10-31 18:37:01');
INSERT INTO `auth_resource` VALUES (2104, 2100, 'cloud_maf_bi', '销售报表', 'function', NULL, '/reportForm/sales', NULL, NULL, 1, '210-6', NULL, 0, '2019-10-31 18:20:46', '2019-10-31 18:37:03');
INSERT INTO `auth_resource` VALUES (2105, 2100, 'cloud_maf_bi', '产量分析', 'function', NULL, '/reportForm/yieldAnalysis', NULL, NULL, 1, '210-7', NULL, 0, '2019-10-31 18:20:46', '2019-10-31 18:37:06');
INSERT INTO `auth_resource` VALUES (2106, 2100, 'cloud_maf_bi', '物流报表', 'function', NULL, '/reportForm/logistics', NULL, NULL, 1, '210-8', NULL, 0, '2019-10-31 18:20:46', '2019-10-31 18:37:08');
INSERT INTO `auth_resource` VALUES (2107, 2100, 'cloud_maf_bi', '合格率', 'function', NULL, '/reportForm/passRate', NULL, NULL, 1, '210-9', NULL, 0, '2019-10-31 18:20:46', '2019-10-31 18:37:11');
INSERT INTO `auth_resource` VALUES (2108, 2100, 'cloud_maf_bi', '产量、消耗分析表', 'function', NULL, '/reportForm/productConsumptAnaly', NULL, NULL, 1, '210-10', NULL, 0, '2019-10-31 18:20:46', '2019-10-31 18:37:14');
INSERT INTO `auth_resource` VALUES (2190, 0, 'cloud_maf_bi', '采购管理', 'menu', NULL, '/purchase', NULL, NULL, 0, '200-2', NULL, 0, '2019-11-28 17:51:49', '2019-11-28 18:27:34');
INSERT INTO `auth_resource` VALUES (2200, 2190, 'cloud_maf_bi', '采购订单', 'function', NULL, '/purchase/order', NULL, NULL, 1, '200-2-1', NULL, 0, '2019-10-31 18:20:46', '2019-11-28 19:02:40');
INSERT INTO `auth_resource` VALUES (2205, 2200, 'cloud_maf_bi', '采购列表', 'page', NULL, '/purchase/order/index', NULL, NULL, 1, '200-2-1-1', NULL, 0, '2019-11-28 17:53:12', '2019-11-28 18:27:43');
INSERT INTO `auth_resource` VALUES (2210, 2200, 'cloud_maf_bi', '订单详情', 'page', NULL, '/purchase/order/detail', NULL, NULL, 0, '200-2-1-2', NULL, 0, '2019-10-31 18:20:46', '2019-11-28 19:02:43');
INSERT INTO `auth_resource` VALUES (2220, 2200, 'cloud_maf_bi', '新增合同', 'page', NULL, '/purchase/order/add', NULL, NULL, 0, '200-2-1-3', NULL, 0, '2019-10-31 18:20:46', '2019-11-28 19:02:47');
INSERT INTO `auth_resource` VALUES (2250, 2190, 'cloud_maf_bi', '采购结算', 'function', NULL, '/purchase/settle', NULL, NULL, 0, '200-2-2', NULL, 0, '2019-11-28 17:55:10', '2019-11-28 18:28:15');
INSERT INTO `auth_resource` VALUES (2300, 0, 'cloud_maf_bi', '销售订单', 'function', NULL, '/sale', NULL, NULL, 1, '200-3', NULL, 0, '2019-10-31 18:20:46', '2019-11-08 02:40:24');
INSERT INTO `auth_resource` VALUES (2310, 2300, 'cloud_maf_bi', '订单详情', 'page', NULL, '/sale/detail', NULL, NULL, 0, '200-3-1', NULL, 0, '2019-10-31 18:20:46', '2019-11-08 09:52:13');
INSERT INTO `auth_resource` VALUES (2320, 2300, 'cloud_maf_bi', '新增合同', 'page', NULL, '/sale/add', NULL, NULL, 0, '200-3-2', NULL, 0, '2019-10-31 18:20:46', '2019-11-08 09:52:16');
INSERT INTO `auth_resource` VALUES (2395, 0, 'cloud_maf_bi', '物流管理', 'menu', NULL, '/logisticsManage', NULL, NULL, 0, '200-4', NULL, 0, '2019-11-28 18:08:38', '2019-11-28 19:00:15');
INSERT INTO `auth_resource` VALUES (2396, 2395, 'cloud_maf_bi', '合同外磅单', 'function', NULL, '/logisticsManage/contractStatement', NULL, NULL, 1, '200-7', NULL, 0, '2019-11-28 18:58:35', '2019-11-28 18:58:58');
INSERT INTO `auth_resource` VALUES (2400, 2395, 'cloud_maf_bi', '物流订单', 'function', NULL, '/logisticsManage/order', NULL, NULL, 1, '200-4-1', NULL, 0, '2019-10-31 18:20:46', '2019-11-28 19:14:09');
INSERT INTO `auth_resource` VALUES (2405, 2400, 'cloud_maf_bi', '订单列表', 'page', NULL, '/logisticsManage/order/index', NULL, NULL, 0, '200-4-1-1', NULL, 0, '2019-11-28 18:10:32', '2019-11-28 18:29:09');
INSERT INTO `auth_resource` VALUES (2410, 2400, 'cloud_maf_bi', '订单详情', 'page', NULL, '/logisticsManage/order/detail', NULL, NULL, 0, '200-4-1-2', NULL, 0, '2019-10-31 18:20:46', '2019-11-28 19:14:30');
INSERT INTO `auth_resource` VALUES (2420, 2400, 'cloud_maf_bi', '承运商详情', 'page', NULL, '/logisticsManage/order/transportDetail', NULL, NULL, 0, '200-4-1-3', NULL, 0, '2019-10-31 18:20:46', '2019-11-28 19:14:34');
INSERT INTO `auth_resource` VALUES (2500, 0, 'cloud_maf_bi', '库存管理', 'menu', NULL, '/inventoryManage', NULL, NULL, 0, '200-5', NULL, 0, '2019-11-13 10:19:19', '2019-11-13 10:32:12');
INSERT INTO `auth_resource` VALUES (2510, 2500, 'cloud_maf_bi', '地磅单', 'function', NULL, '/inventoryManage/weightMachine', NULL, NULL, 1, '200-5-1', NULL, 0, '2019-11-13 10:20:11', '2019-11-13 10:32:55');
INSERT INTO `auth_resource` VALUES (2520, 2500, 'cloud_maf_bi', '采购入库', 'function', NULL, '/inventoryManage/purchaseWarehousing', NULL, NULL, 1, '200-5-2', NULL, 0, '2019-11-28 17:58:48', '2019-11-28 18:52:53');
INSERT INTO `auth_resource` VALUES (2521, 2520, 'cloud_maf_bi', '采购列表', 'page', NULL, '/inventoryManage/purchaseWarehousing/index', NULL, NULL, 0, '200-5-2-1', NULL, 0, '2019-11-28 17:59:40', '2019-11-28 18:29:47');
INSERT INTO `auth_resource` VALUES (2522, 2520, 'cloud_maf_bi', '采购详情', 'page', NULL, '/inventoryManage/purchaseWarehousing/detail', NULL, NULL, 1, '200-5-2-2', NULL, 0, '2019-11-28 18:01:02', '2019-11-28 18:29:50');
INSERT INTO `auth_resource` VALUES (2530, 2500, 'cloud_maf_bi', '成品入库', 'function', NULL, '/inventoryManage/productStorage', NULL, NULL, 0, '200-5-3', NULL, 0, '2019-11-28 18:01:59', '2019-11-28 18:30:18');
INSERT INTO `auth_resource` VALUES (2540, 2500, 'cloud_maf_bi', '销售出库', 'function', NULL, '/inventoryManage/salesOutStock', NULL, NULL, 1, '200-5-4', NULL, 0, '2019-11-28 18:02:58', '2019-11-28 18:53:00');
INSERT INTO `auth_resource` VALUES (2541, 2540, 'cloud_maf_bi', '销售列表', 'page', NULL, '/inventoryManage/salesOutStock/index', NULL, NULL, 0, '200-5-4-1', NULL, 0, '2019-11-28 18:03:39', '2019-11-28 18:30:30');
INSERT INTO `auth_resource` VALUES (2542, 2540, 'cloud_maf_bi', '销售详情', 'page', NULL, '/inventoryManage/salesOutStock/detail', NULL, NULL, 1, '200-5-4-2', NULL, 0, '2019-11-28 18:04:13', '2019-11-28 18:30:33');
INSERT INTO `auth_resource` VALUES (2550, 2500, 'cloud_maf_bi', '现存量查询', 'function', NULL, '/inventoryManage/existingQuantityQuery', NULL, NULL, 0, '200-5-5', NULL, 0, '2019-11-28 18:04:54', '2019-11-28 18:30:41');
INSERT INTO `auth_resource` VALUES (2560, 2500, 'cloud_maf_bi', '盘点管理', 'function', NULL, '/inventoryManage/inventory', NULL, NULL, 1, '200-5-6', NULL, 0, '2019-11-28 18:05:39', '2019-11-28 18:30:46');
INSERT INTO `auth_resource` VALUES (2561, 2560, 'cloud_maf_bi', '盘点列表', 'page', NULL, '/inventoryManage/inventory/index', NULL, NULL, 0, '200-5-6-1', NULL, 0, '2019-11-28 18:06:31', '2019-11-28 18:30:58');
INSERT INTO `auth_resource` VALUES (2562, 2560, 'cloud_maf_bi', '新增盘点单', 'page', NULL, '/inventoryManage/inventory/Edit', NULL, NULL, 0, '200-5-6-2', NULL, 0, '2019-11-28 18:07:06', '2019-11-28 18:31:03');
INSERT INTO `auth_resource` VALUES (2563, 2560, 'cloud_maf_bi', '盘点详情', 'page', NULL, '/inventoryManage/inventory/detail', NULL, NULL, 0, '200-5-6-3', NULL, 0, '2019-11-28 18:07:37', '2019-11-28 18:31:07');
INSERT INTO `auth_resource` VALUES (2600, 0, 'cloud_maf_bi', '磅房管理', 'menu', NULL, '/weighbridge', NULL, NULL, 0, '200-6', NULL, 0, '2019-11-28 18:11:59', '2019-11-28 18:31:14');
INSERT INTO `auth_resource` VALUES (2610, 2600, 'cloud_maf_bi', '采购过磅', 'function', NULL, '/weighbridge/purchase', NULL, NULL, 1, '200-6-1', NULL, 0, '2019-11-28 18:12:40', '2019-11-28 18:31:24');
INSERT INTO `auth_resource` VALUES (2611, 2610, 'cloud_maf_bi', '过磅车辆列表', 'page', NULL, '/weighbridge/purchase/index', NULL, NULL, 0, '200-6-1-1', NULL, 0, '2019-11-28 18:21:43', '2019-11-28 18:31:45');
INSERT INTO `auth_resource` VALUES (2612, 2610, 'cloud_maf_bi', '新增称重', 'page', NULL, '/weighbridge/purchase/add', NULL, NULL, 0, '200-6-1-2', NULL, 0, '2019-11-28 18:22:10', '2019-11-28 18:31:48');
INSERT INTO `auth_resource` VALUES (2613, 2610, 'cloud_maf_bi', '开始称重', 'page', NULL, '/weighbridge/purchase/edit', NULL, NULL, 0, '200-6-1-3', NULL, 0, '2019-11-28 18:22:49', '2019-11-28 18:31:51');
INSERT INTO `auth_resource` VALUES (2614, 2610, 'cloud_maf_bi', '磅单详情', 'page', NULL, '/weighbridge/purchase/detail', NULL, NULL, 0, '200-6-1-4', NULL, 0, '2019-11-28 18:23:28', '2019-11-28 18:31:54');
INSERT INTO `auth_resource` VALUES (2670, 2600, 'cloud_maf_bi', '销售过磅', 'function', NULL, '/weighbridge/sale', NULL, NULL, 1, '200-6-2', NULL, 0, '2019-11-28 18:24:06', '2019-11-28 18:32:00');
INSERT INTO `auth_resource` VALUES (2671, 2670, 'cloud_maf_bi', '过磅车辆列表', 'page', NULL, '/weighbridge/sale/index', NULL, NULL, 0, '200-6-2-1', NULL, 0, '2019-11-28 18:24:52', '2019-11-28 18:32:12');
INSERT INTO `auth_resource` VALUES (2672, 2670, 'cloud_maf_bi', '新增称重', 'page', NULL, '/weighbridge/sale/add', NULL, NULL, 0, '200-6-2-2', NULL, 0, '2019-11-28 18:25:36', '2019-11-28 18:32:14');
INSERT INTO `auth_resource` VALUES (2673, 2670, 'cloud_maf_bi', '开始称重', 'page', NULL, '/weighbridge/sale/edit', NULL, NULL, 0, '200-6-2-3', NULL, 0, '2019-11-28 18:26:07', '2019-11-28 18:32:18');
INSERT INTO `auth_resource` VALUES (2674, 2670, 'cloud_maf_bi', '磅单详情', 'page', NULL, '/weighbridge/sale/detail', NULL, NULL, 0, '200-6-2-4', NULL, 0, '2019-11-28 18:26:35', '2019-11-28 18:32:21');


-- ----------------------------
-- BI菜单相关
-- ----------------------------
INSERT INTO `user_cloudmanufacture`.`auth_role_resource`( `role_id`, `resource_id`, `order_num`, `deleted`, `create_user`, `create_time`, `update_time`) VALUES ( 1, 2190, NULL, 0, 1, '2019-11-28 15:44:26', '2019-11-28 15:44:26');
INSERT INTO `user_cloudmanufacture`.`auth_role_resource`( `role_id`, `resource_id`, `order_num`, `deleted`, `create_user`, `create_time`, `update_time`) VALUES (1, 2205, NULL, 0, 1, '2019-11-28 15:44:26', '2019-11-28 15:44:26');
INSERT INTO `user_cloudmanufacture`.`auth_role_resource`( `role_id`, `resource_id`, `order_num`, `deleted`, `create_user`, `create_time`, `update_time`) VALUES (1, 2250, NULL, 0, 1, '2019-11-28 15:44:26', '2019-11-28 15:44:26');
INSERT INTO `user_cloudmanufacture`.`auth_role_resource`( `role_id`, `resource_id`, `order_num`, `deleted`, `create_user`, `create_time`, `update_time`) VALUES (1, 2395, NULL, 0, 1, '2019-11-28 15:44:26', '2019-11-28 15:44:26');
INSERT INTO `user_cloudmanufacture`.`auth_role_resource`( `role_id`, `resource_id`, `order_num`, `deleted`, `create_user`, `create_time`, `update_time`) VALUES (1, 2405, NULL, 0, 1, '2019-11-28 15:44:26', '2019-11-28 15:44:26');
INSERT INTO `user_cloudmanufacture`.`auth_role_resource`( `role_id`, `resource_id`, `order_num`, `deleted`, `create_user`, `create_time`, `update_time`) VALUES (1, 2520, NULL, 0, 1, '2019-11-28 15:44:26', '2019-11-28 15:44:26');
INSERT INTO `user_cloudmanufacture`.`auth_role_resource`( `role_id`, `resource_id`, `order_num`, `deleted`, `create_user`, `create_time`, `update_time`) VALUES (1, 2521, NULL, 0, 1, '2019-11-28 15:44:26', '2019-11-28 15:44:26');
INSERT INTO `user_cloudmanufacture`.`auth_role_resource`( `role_id`, `resource_id`, `order_num`, `deleted`, `create_user`, `create_time`, `update_time`) VALUES (1, 2522, NULL, 0, 1, '2019-11-28 15:44:26', '2019-11-28 15:44:26');
INSERT INTO `user_cloudmanufacture`.`auth_role_resource`( `role_id`, `resource_id`, `order_num`, `deleted`, `create_user`, `create_time`, `update_time`) VALUES (1, 2530, NULL, 0, 1, '2019-11-28 15:44:26', '2019-11-28 15:44:26');
INSERT INTO `user_cloudmanufacture`.`auth_role_resource`( `role_id`, `resource_id`, `order_num`, `deleted`, `create_user`, `create_time`, `update_time`) VALUES (1, 2540, NULL, 0, 1, '2019-11-28 15:44:26', '2019-11-28 15:44:26');
INSERT INTO `user_cloudmanufacture`.`auth_role_resource`( `role_id`, `resource_id`, `order_num`, `deleted`, `create_user`, `create_time`, `update_time`) VALUES (1, 2541, NULL, 0, 1, '2019-11-28 15:44:26', '2019-11-28 15:44:26');
INSERT INTO `user_cloudmanufacture`.`auth_role_resource`( `role_id`, `resource_id`, `order_num`, `deleted`, `create_user`, `create_time`, `update_time`) VALUES (1, 2542, NULL, 0, 1, '2019-11-28 15:44:26', '2019-11-28 15:44:26');
INSERT INTO `user_cloudmanufacture`.`auth_role_resource`( `role_id`, `resource_id`, `order_num`, `deleted`, `create_user`, `create_time`, `update_time`) VALUES (1, 2550, NULL, 0, 1, '2019-11-28 15:44:26', '2019-11-28 15:44:26');
INSERT INTO `user_cloudmanufacture`.`auth_role_resource`( `role_id`, `resource_id`, `order_num`, `deleted`, `create_user`, `create_time`, `update_time`) VALUES (1, 2560, NULL, 0, 1, '2019-11-28 15:44:26', '2019-11-28 15:44:26');
INSERT INTO `user_cloudmanufacture`.`auth_role_resource`( `role_id`, `resource_id`, `order_num`, `deleted`, `create_user`, `create_time`, `update_time`) VALUES (1, 2561, NULL, 0, 1, '2019-11-28 15:44:26', '2019-11-28 15:44:26');
INSERT INTO `user_cloudmanufacture`.`auth_role_resource`( `role_id`, `resource_id`, `order_num`, `deleted`, `create_user`, `create_time`, `update_time`) VALUES (1, 2562, NULL, 0, 1, '2019-11-28 15:44:26', '2019-11-28 15:44:26');
INSERT INTO `user_cloudmanufacture`.`auth_role_resource`( `role_id`, `resource_id`, `order_num`, `deleted`, `create_user`, `create_time`, `update_time`) VALUES (1, 2563, NULL, 0, 1, '2019-11-28 15:44:26', '2019-11-28 15:44:26');
INSERT INTO `user_cloudmanufacture`.`auth_role_resource`( `role_id`, `resource_id`, `order_num`, `deleted`, `create_user`, `create_time`, `update_time`) VALUES (1, 2600, NULL, 0, 1, '2019-11-28 15:44:26', '2019-11-28 15:44:26');
INSERT INTO `user_cloudmanufacture`.`auth_role_resource`( `role_id`, `resource_id`, `order_num`, `deleted`, `create_user`, `create_time`, `update_time`) VALUES (1, 2610, NULL, 0, 1, '2019-11-28 15:44:26', '2019-11-28 15:44:26');
INSERT INTO `user_cloudmanufacture`.`auth_role_resource`( `role_id`, `resource_id`, `order_num`, `deleted`, `create_user`, `create_time`, `update_time`) VALUES (1, 2611, NULL, 0, 1, '2019-11-28 15:44:26', '2019-11-28 15:44:26');
INSERT INTO `user_cloudmanufacture`.`auth_role_resource`( `role_id`, `resource_id`, `order_num`, `deleted`, `create_user`, `create_time`, `update_time`) VALUES (1, 2612, NULL, 0, 1, '2019-11-28 15:44:26', '2019-11-28 15:44:26');
INSERT INTO `user_cloudmanufacture`.`auth_role_resource`( `role_id`, `resource_id`, `order_num`, `deleted`, `create_user`, `create_time`, `update_time`) VALUES (1, 2613, NULL, 0, 1, '2019-11-28 15:44:26', '2019-11-28 15:44:26');
INSERT INTO `user_cloudmanufacture`.`auth_role_resource`( `role_id`, `resource_id`, `order_num`, `deleted`, `create_user`, `create_time`, `update_time`) VALUES (1, 2614, NULL, 0, 1, '2019-11-28 15:44:26', '2019-11-28 15:44:26');
INSERT INTO `user_cloudmanufacture`.`auth_role_resource`( `role_id`, `resource_id`, `order_num`, `deleted`, `create_user`, `create_time`, `update_time`) VALUES (1, 2670, NULL, 0, 1, '2019-11-28 15:44:26', '2019-11-28 15:44:26');
INSERT INTO `user_cloudmanufacture`.`auth_role_resource`( `role_id`, `resource_id`, `order_num`, `deleted`, `create_user`, `create_time`, `update_time`) VALUES (1, 2671, NULL, 0, 1, '2019-11-28 15:44:26', '2019-11-28 15:44:26');
INSERT INTO `user_cloudmanufacture`.`auth_role_resource`( `role_id`, `resource_id`, `order_num`, `deleted`, `create_user`, `create_time`, `update_time`) VALUES (1, 2672, NULL, 0, 1, '2019-11-28 15:44:26', '2019-11-28 15:44:26');
INSERT INTO `user_cloudmanufacture`.`auth_role_resource`( `role_id`, `resource_id`, `order_num`, `deleted`, `create_user`, `create_time`, `update_time`) VALUES (1, 2673, NULL, 0, 1, '2019-11-28 15:44:26', '2019-11-28 15:44:26');
INSERT INTO `user_cloudmanufacture`.`auth_role_resource`( `role_id`, `resource_id`, `order_num`, `deleted`, `create_user`, `create_time`, `update_time`) VALUES (1, 2674, NULL, 0, 1, '2019-11-28 15:44:26', '2019-11-28 15:44:26');
INSERT INTO `user_cloudmanufacture`.`auth_role_resource`( `role_id`, `resource_id`, `order_num`, `deleted`, `create_user`, `create_time`, `update_time`) VALUES (10, 2190, NULL, 0, 1, '2019-11-28 15:44:26', '2019-11-28 15:44:26');
INSERT INTO `user_cloudmanufacture`.`auth_role_resource`( `role_id`, `resource_id`, `order_num`, `deleted`, `create_user`, `create_time`, `update_time`) VALUES (10, 2205, NULL, 0, 1, '2019-11-28 15:44:26', '2019-11-28 15:44:26');
INSERT INTO `user_cloudmanufacture`.`auth_role_resource`( `role_id`, `resource_id`, `order_num`, `deleted`, `create_user`, `create_time`, `update_time`) VALUES (10, 2250, NULL, 0, 1, '2019-11-28 15:44:26', '2019-11-28 15:44:26');
INSERT INTO `user_cloudmanufacture`.`auth_role_resource`( `role_id`, `resource_id`, `order_num`, `deleted`, `create_user`, `create_time`, `update_time`) VALUES (10, 2395, NULL, 0, 1, '2019-11-28 15:44:26', '2019-11-28 15:44:26');
INSERT INTO `user_cloudmanufacture`.`auth_role_resource`( `role_id`, `resource_id`, `order_num`, `deleted`, `create_user`, `create_time`, `update_time`) VALUES (10, 2405, NULL, 0, 1, '2019-11-28 15:44:26', '2019-11-28 15:44:26');
INSERT INTO `user_cloudmanufacture`.`auth_role_resource`( `role_id`, `resource_id`, `order_num`, `deleted`, `create_user`, `create_time`, `update_time`) VALUES (10, 2520, NULL, 0, 1, '2019-11-28 15:44:26', '2019-11-28 15:44:26');
INSERT INTO `user_cloudmanufacture`.`auth_role_resource`( `role_id`, `resource_id`, `order_num`, `deleted`, `create_user`, `create_time`, `update_time`) VALUES (10, 2521, NULL, 0, 1, '2019-11-28 15:44:26', '2019-11-28 15:44:26');
INSERT INTO `user_cloudmanufacture`.`auth_role_resource`( `role_id`, `resource_id`, `order_num`, `deleted`, `create_user`, `create_time`, `update_time`) VALUES (10, 2522, NULL, 0, 1, '2019-11-28 15:44:26', '2019-11-28 15:44:26');
INSERT INTO `user_cloudmanufacture`.`auth_role_resource`( `role_id`, `resource_id`, `order_num`, `deleted`, `create_user`, `create_time`, `update_time`) VALUES (10, 2530, NULL, 0, 1, '2019-11-28 15:44:26', '2019-11-28 15:44:26');
INSERT INTO `user_cloudmanufacture`.`auth_role_resource`( `role_id`, `resource_id`, `order_num`, `deleted`, `create_user`, `create_time`, `update_time`) VALUES (10, 2540, NULL, 0, 1, '2019-11-28 15:44:26', '2019-11-28 15:44:26');
INSERT INTO `user_cloudmanufacture`.`auth_role_resource`( `role_id`, `resource_id`, `order_num`, `deleted`, `create_user`, `create_time`, `update_time`) VALUES (10, 2541, NULL, 0, 1, '2019-11-28 15:44:26', '2019-11-28 15:44:26');
INSERT INTO `user_cloudmanufacture`.`auth_role_resource`( `role_id`, `resource_id`, `order_num`, `deleted`, `create_user`, `create_time`, `update_time`) VALUES (10, 2542, NULL, 0, 1, '2019-11-28 15:44:26', '2019-11-28 15:44:26');
INSERT INTO `user_cloudmanufacture`.`auth_role_resource`( `role_id`, `resource_id`, `order_num`, `deleted`, `create_user`, `create_time`, `update_time`) VALUES (10, 2550, NULL, 0, 1, '2019-11-28 15:44:26', '2019-11-28 15:44:26');
INSERT INTO `user_cloudmanufacture`.`auth_role_resource`( `role_id`, `resource_id`, `order_num`, `deleted`, `create_user`, `create_time`, `update_time`) VALUES (10, 2560, NULL, 0, 1, '2019-11-28 15:44:26', '2019-11-28 15:44:26');
INSERT INTO `user_cloudmanufacture`.`auth_role_resource`( `role_id`, `resource_id`, `order_num`, `deleted`, `create_user`, `create_time`, `update_time`) VALUES (10, 2561, NULL, 0, 1, '2019-11-28 15:44:26', '2019-11-28 15:44:26');
INSERT INTO `user_cloudmanufacture`.`auth_role_resource`( `role_id`, `resource_id`, `order_num`, `deleted`, `create_user`, `create_time`, `update_time`) VALUES (10, 2562, NULL, 0, 1, '2019-11-28 15:44:26', '2019-11-28 15:44:26');
INSERT INTO `user_cloudmanufacture`.`auth_role_resource`( `role_id`, `resource_id`, `order_num`, `deleted`, `create_user`, `create_time`, `update_time`) VALUES (10, 2563, NULL, 0, 1, '2019-11-28 15:44:26', '2019-11-28 15:44:26');
INSERT INTO `user_cloudmanufacture`.`auth_role_resource`( `role_id`, `resource_id`, `order_num`, `deleted`, `create_user`, `create_time`, `update_time`) VALUES (10, 2600, NULL, 0, 1, '2019-11-28 15:44:26', '2019-11-28 15:44:26');
INSERT INTO `user_cloudmanufacture`.`auth_role_resource`( `role_id`, `resource_id`, `order_num`, `deleted`, `create_user`, `create_time`, `update_time`) VALUES (10, 2610, NULL, 0, 1, '2019-11-28 15:44:26', '2019-11-28 15:44:26');
INSERT INTO `user_cloudmanufacture`.`auth_role_resource`( `role_id`, `resource_id`, `order_num`, `deleted`, `create_user`, `create_time`, `update_time`) VALUES (10, 2611, NULL, 0, 1, '2019-11-28 15:44:26', '2019-11-28 15:44:26');
INSERT INTO `user_cloudmanufacture`.`auth_role_resource`( `role_id`, `resource_id`, `order_num`, `deleted`, `create_user`, `create_time`, `update_time`) VALUES (10, 2612, NULL, 0, 1, '2019-11-28 15:44:26', '2019-11-28 15:44:26');
INSERT INTO `user_cloudmanufacture`.`auth_role_resource`( `role_id`, `resource_id`, `order_num`, `deleted`, `create_user`, `create_time`, `update_time`) VALUES (10, 2613, NULL, 0, 1, '2019-11-28 15:44:26', '2019-11-28 15:44:26');
INSERT INTO `user_cloudmanufacture`.`auth_role_resource`( `role_id`, `resource_id`, `order_num`, `deleted`, `create_user`, `create_time`, `update_time`) VALUES (10, 2614, NULL, 0, 1, '2019-11-28 15:44:26', '2019-11-28 15:44:26');
INSERT INTO `user_cloudmanufacture`.`auth_role_resource`( `role_id`, `resource_id`, `order_num`, `deleted`, `create_user`, `create_time`, `update_time`) VALUES (10, 2670, NULL, 0, 1, '2019-11-28 15:44:26', '2019-11-28 15:44:26');
INSERT INTO `user_cloudmanufacture`.`auth_role_resource`( `role_id`, `resource_id`, `order_num`, `deleted`, `create_user`, `create_time`, `update_time`) VALUES (10, 2671, NULL, 0, 1, '2019-11-28 15:44:26', '2019-11-28 15:44:26');
INSERT INTO `user_cloudmanufacture`.`auth_role_resource`( `role_id`, `resource_id`, `order_num`, `deleted`, `create_user`, `create_time`, `update_time`) VALUES (10, 2672, NULL, 0, 1, '2019-11-28 15:44:26', '2019-11-28 15:44:26');
INSERT INTO `user_cloudmanufacture`.`auth_role_resource`( `role_id`, `resource_id`, `order_num`, `deleted`, `create_user`, `create_time`, `update_time`) VALUES (10, 2673, NULL, 0, 1, '2019-11-28 15:44:26', '2019-11-28 15:44:26');
INSERT INTO `user_cloudmanufacture`.`auth_role_resource`( `role_id`, `resource_id`, `order_num`, `deleted`, `create_user`, `create_time`, `update_time`) VALUES (10, 2674, NULL, 0, 1, '2019-11-28 15:44:26', '2019-11-28 15:44:26');
INSERT INTO `user_cloudmanufacture`.`auth_role_resource`( `role_id`, `resource_id`, `order_num`, `deleted`, `create_user`, `create_time`, `update_time`) VALUES (1, 2396, NULL, 0, 1, '2019-11-28 19:01:27', '2019-11-28 19:01:28');
INSERT INTO `user_cloudmanufacture`.`auth_role_resource`( `role_id`, `resource_id`, `order_num`, `deleted`, `create_user`, `create_time`, `update_time`) VALUES (10, 2396, NULL, 0, 1, '2019-11-28 19:01:35', '2019-11-28 19:01:37');
