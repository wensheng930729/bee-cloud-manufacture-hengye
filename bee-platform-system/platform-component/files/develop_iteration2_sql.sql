
-- 迭代2数据库脚本

-- 承运方运输地点相关修改
ALTER TABLE `si_cloudmanufacture`.`buy_carrier_transport`
ADD COLUMN `starting_place_id` int(10) NULL DEFAULT NULL COMMENT '起始地地点id' AFTER `to_factory`,
ADD COLUMN `arrival_place_id` int(10) NULL DEFAULT NULL COMMENT '到达地地点id' AFTER `starting_place`;

ALTER TABLE `si_cloudmanufacture`.`sale_carrier_transport`
ADD COLUMN `starting_place_id` int(10) NULL DEFAULT NULL COMMENT '起始地地点id' AFTER `to_factory`,
ADD COLUMN `arrival_place_id` int(10) NULL DEFAULT NULL COMMENT '到达地地点id' AFTER `starting_place`;


-- 网关配置表修改
ALTER TABLE `si_cloudmanufacture`.`plc_factory_gateway`
ADD COLUMN `deleted` tinyint(1) COMMENT '是否删除' AFTER `status`,
ADD COLUMN `create_id` int(10) DEFAULT NULL COMMENT '创建人id' AFTER `deleted`,
ADD COLUMN `creator` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '创建人名称' AFTER `create_id`,
ADD COLUMN `create_time` datetime(0) DEFAULT NULL COMMENT '创建时间' AFTER `creator`,
ADD COLUMN `modify_id` int(10) DEFAULT NULL COMMENT '修改人id' AFTER `create_time`,
ADD COLUMN `modifier` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '修改人名称' AFTER `modify_id`,
ADD COLUMN `modify_time` datetime(0) DEFAULT NULL COMMENT '修改时间' AFTER `modifier`;

-- 运输车次添加榜单关联
ALTER TABLE `si_cloudmanufacture`.`buy_carrier_transport_detail`
ADD COLUMN `machine_id` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NULL DEFAULT NULL COMMENT '磅单业务id' AFTER `arrival_status`;

-- 采购入库添加实际入库数量字段
ALTER TABLE `buy_product_pending_storage`
ADD COLUMN `actual_product_number`  decimal(4,0) NULL COMMENT '实际入库数量' AFTER `product_name`;

-- 新增出库修改产品数量保留小数点后面位数
ALTER TABLE `pick_out_storage_detail`
MODIFY COLUMN `product_number`  decimal(10,4) NOT NULL COMMENT '领用数量' AFTER `product_id`;

-- 采购入库添加实际入库数量字段
ALTER TABLE `buy_product_pending_storage`
ADD COLUMN `actual_product_number`  decimal(4,0) NULL AFTER `product_name`;

-- 合同表添加数量统计字段
ALTER TABLE `si_cloudmanufacture`.`buy_contract_basic`
ADD COLUMN `issued_volume` decimal(20, 4) NULL DEFAULT NULL COMMENT '已发量' AFTER `arrival_volume`,
ADD COLUMN `undelivered_volume` decimal(20, 4) NULL DEFAULT NULL COMMENT '未发量' AFTER `issued_volume`;
ALTER TABLE `si_cloudmanufacture`.`sale_contract_basic`
ADD COLUMN `issued_volume` decimal(20, 4) NULL DEFAULT NULL COMMENT '已发量' AFTER `received_volume`,
ADD COLUMN `undelivered_volume` decimal(20, 4) NULL DEFAULT NULL COMMENT '未发量' AFTER `issued_volume`;
-- 设置历史数据默认值
update buy_contract_basic t set t.issued_volume = 0 where t.issued_volume is null;
update buy_contract_basic t set t.undelivered_volume = 0 where t.undelivered_volume is null;
update sale_contract_basic t set t.issued_volume = 0 where t.issued_volume is null;
update sale_contract_basic t set t.undelivered_volume = 0 where t.undelivered_volume is null;

-- 11.08上线脚本
-- 运输段添加字段
ALTER TABLE `si_cloudmanufacture`.`buy_transport_section`
ADD COLUMN `to_factory` int(2) NULL DEFAULT 0 COMMENT '是否到厂(0-不到厂 1-到厂)' AFTER `transport_mode`,
ADD COLUMN `starting_place_id` int(10) NULL DEFAULT NULL COMMENT '起始地地点id' AFTER `to_factory`,
ADD COLUMN `starting_place` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NULL DEFAULT NULL COMMENT '起始地' AFTER `starting_place_id`,
ADD COLUMN `arrival_place_id` int(10) NULL DEFAULT NULL COMMENT '到达地地点id' AFTER `starting_place`,
ADD COLUMN `arrival_place` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NULL DEFAULT NULL COMMENT '到达地' AFTER `arrival_place_id`;
ALTER TABLE `si_cloudmanufacture`.`sale_transport_section`
ADD COLUMN `to_factory` int(2) NULL DEFAULT 0 COMMENT '是否到厂(0-不到厂 1-到厂)' AFTER `transport_mode`,
ADD COLUMN `starting_place_id` int(10) NULL DEFAULT NULL COMMENT '起始地地点id' AFTER `to_factory`,
ADD COLUMN `starting_place` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NULL DEFAULT NULL COMMENT '起始地' AFTER `starting_place_id`,
ADD COLUMN `arrival_place_id` int(10) NULL DEFAULT NULL COMMENT '到达地地点id' AFTER `starting_place`,
ADD COLUMN `arrival_place` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NULL DEFAULT NULL COMMENT '到达地' AFTER `arrival_place_id`;




