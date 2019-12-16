
-- si_cloudmanufacture数据库
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




-- user_cloudmanufacture数据库
INSERT INTO `user_cloudmanufacture`.`auth_resource`(`id`, `pid`, `sub_sys`, `name`, `resource_type`, `icon`, `path`, `component`, `order_num`, `is_hide`, `position`, `show_type`, `deleted`, `create_time`, `update_time`) VALUES (160, 0, 'cloud_maf_app', '二维码查询', 'function', NULL, 'codeQuery', NULL, NULL, 0, '17', NULL, 0, '2019-11-11 11:30:39', '2019-11-11 11:31:11');

INSERT INTO `user_cloudmanufacture`.`auth_role_resource`(`role_id`, `resource_id`, `order_num`, `deleted`, `create_user`, `create_time`, `update_time`) VALUES (1, 160, NULL, 0, 1001, '2019-11-11 11:31:31', NULL);
INSERT INTO `user_cloudmanufacture`.`auth_role_resource`(`role_id`, `resource_id`, `order_num`, `deleted`, `create_user`, `create_time`, `update_time`) VALUES (10, 160, NULL, 0, 1001, '2019-11-11 11:31:41', NULL);

INSERT INTO `user_cloudmanufacture`.`auth_resource`(`id`, `pid`, `sub_sys`, `name`, `resource_type`, `icon`, `path`, `component`, `order_num`, `is_hide`, `position`, `show_type`, `deleted`, `create_time`, `update_time`) VALUES (2500, 0, 'cloud_maf_bi', '库存管理', 'menu', NULL, '/inventoryManage', NULL, NULL, 0, '200-5', NULL, 0, '2019-11-13 10:19:19', '2019-11-13 10:32:12');
INSERT INTO `user_cloudmanufacture`.`auth_resource`(`id`, `pid`, `sub_sys`, `name`, `resource_type`, `icon`, `path`, `component`, `order_num`, `is_hide`, `position`, `show_type`, `deleted`, `create_time`, `update_time`) VALUES (2510, 2500, 'cloud_maf_bi', '地磅单', 'function', NULL, '/inventoryManage/weightMachine', NULL, NULL, 1, '200-5-1', NULL, 0, '2019-11-13 10:20:11', '2019-11-13 10:32:55');

INSERT INTO `user_cloudmanufacture`.`auth_role_resource`(`role_id`, `resource_id`, `order_num`, `deleted`, `create_user`, `create_time`, `update_time`) VALUES (1, 2500, NULL, 0, 1001, '2019-11-13 10:20:38', '2019-11-13 10:20:39');
INSERT INTO `user_cloudmanufacture`.`auth_role_resource`(`role_id`, `resource_id`, `order_num`, `deleted`, `create_user`, `create_time`, `update_time`) VALUES (1, 2510, NULL, 0, 1001, '2019-11-13 10:20:50', '2019-11-13 10:20:52');
INSERT INTO `user_cloudmanufacture`.`auth_role_resource`(`role_id`, `resource_id`, `order_num`, `deleted`, `create_user`, `create_time`, `update_time`) VALUES (10, 2500, NULL, 0, 1001, '2019-11-13 10:26:57', '2019-11-13 10:26:59');
INSERT INTO `user_cloudmanufacture`.`auth_role_resource`(`role_id`, `resource_id`, `order_num`, `deleted`, `create_user`, `create_time`, `update_time`) VALUES (10, 2510, NULL, 0, 1001, '2019-11-13 10:27:09', '2019-11-13 10:27:10');
-- 添加承运商类别字段
ALTER TABLE `user_cloudmanufacture`.`auth_customer_or_supplier`
ADD COLUMN `carrier_category` tinyint(1) NULL COMMENT '承运商类别（0公司 1个人）' AFTER `carrier`;

-- 处理历史数据承运商类别字段值
UPDATE auth_customer_or_supplier set carrier_category=0 WHERE carrier=1 AND deleted=0 AND type=1 AND id not in (123,136);
UPDATE auth_customer_or_supplier set carrier_category=1 WHERE id in (123,136);
