
-- 迭代3数据库脚本

-- 添加磅单打印模板字段
ALTER TABLE `si_cloudmanufacture`.`config_printer_token`
ADD COLUMN `template_id` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT '65870813141efe63' COMMENT '磅单模板id' AFTER `app_key`;

-- 添加地磅司磅员
ALTER TABLE `si_cloudmanufacture`.`buy_weight_machine`
ADD COLUMN `weight_man` varchar(64) NULL COMMENT '司磅员' AFTER `deduct_weight_by_manual`;

ALTER TABLE `si_cloudmanufacture`.`sale_weight_machine`
ADD COLUMN `weight_man` varchar(64) NULL COMMENT '司磅员' AFTER `deduct_weight_by_manual`;

-- 新增承运商id
ALTER TABLE `si_cloudmanufacture`.`finished_product_be_out_of_storage`
ADD COLUMN `carrier_id` bigint(10) NULL DEFAULT NULL COMMENT '承运方id' AFTER `modify_time`;

-- 添加采购待入库磅房备注字段
ALTER TABLE `buy_product_pending_storage`
ADD COLUMN `remark`  varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NULL DEFAULT '' COMMENT '磅房备注' AFTER `storage_name`;


-- 修改注释
ALTER TABLE `si_cloudmanufacture`.`bar_code`
MODIFY COLUMN `type` tinyint(2) NOT NULL COMMENT '类型1样品编码2吨袋编码' AFTER `date`;