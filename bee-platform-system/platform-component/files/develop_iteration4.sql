-- 成品出库，新增表
CREATE TABLE `finished_product_free_out` (
  `id` int(11) NOT NULL AUTO_INCREMENT COMMENT '自增主键',
  `factory_id` int(11) DEFAULT NULL COMMENT '工厂id',
  `org_id` int(11) DEFAULT NULL COMMENT '企业id',
  `contract_car_id` varchar(255) DEFAULT NULL COMMENT '关联出库车辆信息业务id',
  `product_id` int(11) DEFAULT NULL COMMENT '产品id',
  `product_name` varchar(255) DEFAULT NULL COMMENT '产品名称',
  `product_spec_id` int(11) DEFAULT NULL COMMENT '产品规格id',
  `product_spec_name` varchar(255) DEFAULT NULL COMMENT '产品规格名称',
  `product_number` decimal(4,0) DEFAULT NULL COMMENT '出库数量',
  `storage_id` int(11) DEFAULT NULL COMMENT '仓库id',
  `storage_name` varchar(255) DEFAULT NULL COMMENT '仓库名称',
  `status` tinyint(2) DEFAULT '1' COMMENT '数据状态0删除1正常',
  `create_id` int(20) DEFAULT NULL COMMENT '创建人id',
  `creator` varchar(20) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '创建人名称',
  `create_time` datetime DEFAULT NULL COMMENT '创建/申请时间',
  `modify_id` int(20) DEFAULT NULL COMMENT '修改人id',
  `modifier` varchar(20) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '修改人',
  `modify_time` datetime DEFAULT CURRENT_TIMESTAMP COMMENT '修改时间',
  PRIMARY KEY (`id`),
  KEY `contract_car_id` (`contract_car_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='手动输入成品出库记录表';

-- 合同新增类型
ALTER TABLE `si_cloudmanufacture`.`buy_contract_basic`
ADD COLUMN `type` tinyint(1) DEFAULT '0' COMMENT '承运商类别（0短期合同 1长期协议）' AFTER `remark`;

ALTER TABLE `si_cloudmanufacture`.`sale_contract_basic`
ADD COLUMN `type` tinyint(1) DEFAULT '0' COMMENT '承运商类别（0短期合同 1长期协议）' AFTER `remark`;

ALTER TABLE `si_cloudmanufacture`.`sale_weight_machine`
ADD COLUMN `is_push_storage` tinyint(2) NULL COMMENT '是否推送到仓库 0 未推送 1 已推送' AFTER `weight_man`;

ALTER TABLE `si_cloudmanufacture`.`sale_weight_machine`
MODIFY COLUMN `is_push_storage` tinyint(2) NULL DEFAULT 0 COMMENT '是否推送到仓库 0 未推送 1 已推送' AFTER `weight_man`;