-- 磅单新增结算信息
ALTER TABLE `si_cloudmanufacture`.`buy_weight_machine`
ADD COLUMN `settle_status` tinyint(1) DEFAULT '0' COMMENT '0未结算 1已结算' AFTER `weight_man`;

ALTER TABLE `si_cloudmanufacture`.`buy_weight_machine`
ADD COLUMN `settle_time` datetime(0) NULL COMMENT '结算时间' AFTER `settle_status`;


-- web端地磅新增字段
ALTER TABLE `si_cloudmanufacture`.`buy_weight_machine`
ADD COLUMN `contact` varchar(64) NULL COMMENT '联系方式' AFTER `settle_time`,
ADD COLUMN `bind_ignore` tinyint(2) NULL COMMENT '物流绑定是否忽略 0忽略 无效 1 不忽略 有效' AFTER `contact`;
ALTER TABLE `si_cloudmanufacture`.`buy_weight_machine`
MODIFY COLUMN `bind_ignore` tinyint(2) NULL DEFAULT 1 COMMENT '物流绑定是否忽略 0忽略 无效 1 不忽略 有效' AFTER `contact`;

ALTER TABLE `si_cloudmanufacture`.`sale_weight_machine`
ADD COLUMN `contact` varchar(64) NULL COMMENT '联系方式' AFTER `is_push_storage`,
ADD COLUMN `bind_ignore` tinyint(2) NULL COMMENT '物流绑定是否忽略 0忽略 无效 1 不忽略 有效' AFTER `contact`;
ALTER TABLE `si_cloudmanufacture`.`sale_weight_machine`
MODIFY COLUMN `bind_ignore` tinyint(2) NULL DEFAULT 1 COMMENT '物流绑定是否忽略 0忽略 无效 1 不忽略 有效' AFTER `contact`;

-- 出库车辆信息加上联系方式
ALTER TABLE `finished_product_be_out_of_storage`
ADD COLUMN `contact`  varchar(255) NULL COMMENT '联系方式' AFTER `driver_name`;



-- ----------------------------
-- 库存盘点相关表
-- ----------------------------
DROP TABLE IF EXISTS `stock_inventory`;
CREATE TABLE `stock_inventory`  (
  `id` int(10) NOT NULL AUTO_INCREMENT COMMENT '主键',
  `inventory_order_id` varchar(30) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NOT NULL COMMENT '盘点单单号',
  `inventory_name` varchar(60) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '盘点单名称',
  `inventory_type` tinyint(1) DEFAULT NULL COMMENT '盘点分类 。1 全盘 ,2产品分类盘点 ，3 产品盘点 4.仓库盘点',
  `enterprise_id` int(10) DEFAULT NULL COMMENT '企业id',
  `factory_id` int(10) NOT NULL COMMENT '工厂id',
  `remarks` varchar(100) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '备注',
  `status` tinyint(1) NOT NULL COMMENT '是否有效0无效，1有效',
  `immutable` tinyint(1) NOT NULL COMMENT '是否是不可更改的 0 可更改 ，1不可更改',
  `create_id` int(10) NOT NULL COMMENT '创建人id',
  `creator` varchar(60) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NOT NULL COMMENT '创建人姓名',
  `create_time` datetime(0) NOT NULL COMMENT '创建时间',
  `modify_id` int(10) DEFAULT NULL COMMENT '修改人id',
  `modifier` varchar(60) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '修改人姓名',
  `modify_time` datetime(0) DEFAULT NULL COMMENT '修改时间',
  PRIMARY KEY (`id`) USING BTREE,
  UNIQUE INDEX `inventory_index`(`inventory_order_id`) USING BTREE
) ENGINE = InnoDB AUTO_INCREMENT = 2 CHARACTER SET = utf8mb4 COLLATE = utf8mb4_0900_ai_ci COMMENT = '库存盘点主表' ROW_FORMAT = Dynamic;

-- ----------------------------
-- 盘点详细表
-- ----------------------------
DROP TABLE IF EXISTS `stock_inventory_detail`;
CREATE TABLE `stock_inventory_detail`  (
  `id` int(10) NOT NULL AUTO_INCREMENT COMMENT '主键',
  `inventory_order_id` varchar(30) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NOT NULL COMMENT '盘点主表盘点单id',
  `product_id` int(10) DEFAULT NULL COMMENT '产品id',
  `storage_id` int(10) DEFAULT NULL COMMENT '仓库id',
  `product_spec_id` int(10) DEFAULT NULL COMMENT '产品规格id',
  `product_unit` varchar(20) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '产品计量单位',
  `account_num` decimal(10, 3) DEFAULT NULL COMMENT '账面数量',
  `actual_num` decimal(10, 3) DEFAULT NULL COMMENT '实盘数量',
  `difference_num` decimal(10, 3) DEFAULT NULL COMMENT '差异数量',
  `status` tinyint(1) DEFAULT NULL COMMENT '是否有效0无效，1有效',
  `create_id` int(10) DEFAULT NULL COMMENT '创建人id',
  `creator` varchar(60) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '创建人姓名',
  `create_time` datetime(0) DEFAULT NULL COMMENT '创建时间',
  `modify_id` int(10) DEFAULT NULL COMMENT '修改人id',
  `modifier` varchar(60) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '修改人姓名',
  `modify_time` datetime(0) DEFAULT NULL COMMENT '修改时间',
  PRIMARY KEY (`id`) USING BTREE,
  INDEX `inventory_index`(`inventory_order_id`) USING BTREE
) ENGINE = InnoDB AUTO_INCREMENT = 1 CHARACTER SET = utf8mb4 COLLATE = utf8mb4_0900_ai_ci COMMENT = '库存盘点详细' ROW_FORMAT = Dynamic;

-- 版本更新说明
CREATE TABLE `version_upgrade` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT COMMENT '主键',
  `version_num` varchar(20) CHARACTER SET utf8 COLLATE utf8_general_ci NOT NULL COMMENT '版本号',
  `description` varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci NOT NULL COMMENT '描述',
  `upgrade_time` datetime NOT NULL COMMENT '升级时间',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE=InnoDB AUTO_INCREMENT=4 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci ROW_FORMAT=DYNAMIC COMMENT='版本升级描述';

-- 采购结算新增车辆数
ALTER TABLE `si_cloudmanufacture`.`buy_contract_settlement`
ADD COLUMN `car_num` int(2) NULL COMMENT '车辆数' AFTER `modify_time`;
ALTER TABLE `si_cloudmanufacture`.`buy_contract_settlement`
ADD COLUMN `weight_ids` varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci NOT NULL COMMENT '车辆ids' AFTER `car_num`;