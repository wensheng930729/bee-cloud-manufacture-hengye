-- 化验输出项添加小数位数字段
ALTER TABLE `si_cloudmanufacture`.`config_product_test_attribute_out`
ADD COLUMN `decimal_digit` tinyint(1) NULL COMMENT '小数位数（0整数 1 一位小数 2 二位小数 3 三位小数 4 四位小数 5 五位小数）' AFTER `test_unit`;

UPDATE `si_cloudmanufacture`.`config_product_test_attribute_out` set decimal_digit=2;

ALTER TABLE `si_cloudmanufacture`.`config_product_test_attribute_out`
MODIFY COLUMN `decimal_digit` tinyint(1) NULL DEFAULT 2 COMMENT '小数位数（0整数 1 一位小数 2 二位小数 3 三位小数 4 四位小数 5 五位小数）' AFTER `test_unit`;

-- 修改车牌号
ALTER TABLE `si_cloudmanufacture`.`finished_product_be_out_of_storage`
ADD COLUMN `machine_id` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NULL DEFAULT NULL COMMENT '磅单业务id' AFTER `contract_id`;

-- 恒业
CREATE TABLE `sample_assay_result_temporary` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `sample_code` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NOT NULL COMMENT '样品编号',
  `assay_item` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NOT NULL COMMENT '化验项',
  `assay_value` double(20,10) NOT NULL COMMENT '化验结果',
  `test_unit` tinyint(1) DEFAULT '0' COMMENT '化验单位（0 %百分比  1 ‱万分比）',
  `unit_string` varchar(100) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '化验单位名称',
  `business_type` tinyint(2) NOT NULL COMMENT '业务类型1采购2销售3生产4投料',
  `status` tinyint(2) NOT NULL DEFAULT '1' COMMENT '状态0删除1启用',
  `create_id` int(10) NOT NULL COMMENT '创建人id',
  `creator` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NOT NULL COMMENT '创建人',
  `create_time` datetime NOT NULL COMMENT '创建时间',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE=InnoDB AUTO_INCREMENT=275 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci ROW_FORMAT=DYNAMIC COMMENT='样品化验结果临时存储表';

ALTER TABLE `si_cloudmanufacture`.`sample_assay_result_temporary`
ADD COLUMN `unit_string` varchar(100) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NULL DEFAULT NULL COMMENT '化验单位名称' AFTER `test_unit`;
ALTER TABLE `si_cloudmanufacture`.`sample_assay_result`
ADD COLUMN `unit_string` varchar(100) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NULL DEFAULT NULL COMMENT '化验单位名称' AFTER `test_unit`;
ALTER TABLE `si_cloudmanufacture`.`sale_back_assay_result`
ADD COLUMN `unit_string` varchar(100) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NULL DEFAULT NULL COMMENT '化验单位名称' AFTER `test_unit`;
ALTER TABLE `si_cloudmanufacture`.`sample_assay_result`
MODIFY COLUMN `assay_value` double(20, 10) NOT NULL COMMENT '化验结果' AFTER `assay_item`;
ALTER TABLE `si_cloudmanufacture`.`sample_assay_result_temporary`
MODIFY COLUMN `assay_value` double(20, 10) NOT NULL COMMENT '化验结果' AFTER `assay_item`;

UPDATE si_cloudmanufacture.`sample_assay_result_temporary` a SET a.unit_string=IF(a.test_unit=0,'%','‱');
UPDATE si_cloudmanufacture.`sample_assay_result` a SET a.unit_string=IF(a.test_unit=0,'%','‱');
UPDATE si_cloudmanufacture.`sale_back_assay_result` a SET a.unit_string=IF(a.test_unit=0,'%','‱');


ALTER TABLE `buy_weight_machine`
ADD COLUMN `car_deduct_weight`  decimal(16,4) NULL DEFAULT 0.0000 COMMENT '车次货物确认的扣重' AFTER `bind_ignore`;


