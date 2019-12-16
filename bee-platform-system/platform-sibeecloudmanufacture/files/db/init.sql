
##MyBatis-Plus 官网上的初始化SQL：https://mp.baomidou.com/guide/quick-start.html#%E5%88%9D%E5%A7%8B%E5%8C%96%E5%B7%A5%E7%A8%8B

DROP TABLE IF EXISTS ar_user;

CREATE TABLE ar_user
(
	id BIGINT(20) NOT NULL COMMENT '主键ID',
	name VARCHAR(30) NULL DEFAULT NULL COMMENT '姓名',
	age INT(11) NULL DEFAULT NULL COMMENT '年龄',
	email VARCHAR(50) NULL DEFAULT NULL COMMENT '邮箱',
	PRIMARY KEY (id)
);

DELETE FROM ar_user;

INSERT INTO ar_user (id, name, age, email) VALUES
(1, 'Jone', 18, 'test1@baomidou.com'),
(2, 'Jack', 20, 'test2@baomidou.com'),
(3, 'Tom', 28, 'test3@baomidou.com'),
(4, 'Sandy', 21, 'test4@baomidou.com'),
(5, 'Billie', 24, 'test5@baomidou.com');

-- 新增产品规格相关表字段修改

ALTER TABLE `buy_product_pending_storage`
ADD COLUMN `product_spec_id`  int(11) NULL DEFAULT NULL COMMENT '产品规格id' AFTER `product_unit`,
ADD COLUMN `product_spec_name`  varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NULL DEFAULT NULL COMMENT '产品规格名称' AFTER `product_spec_id`;

ALTER TABLE `storage_inventory`
ADD COLUMN `product_spec_id`  int(11) NULL DEFAULT NULL COMMENT '产品规格id' AFTER `product_unit`,
ADD COLUMN `product_spec_name`  varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NULL DEFAULT NULL COMMENT '产品规格名称' AFTER `product_spec_id`;

ALTER TABLE `finished_product_pending_storage`
ADD COLUMN `product_spec_id`  int(11) NULL DEFAULT NULL COMMENT '产品规格id' AFTER `product_unit`,
ADD COLUMN `product_spec_name`  varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NULL DEFAULT NULL COMMENT '产品规格名称' AFTER `product_spec_id`;

ALTER TABLE `finished_product_out_storage_detail`
ADD COLUMN `product_spec_id`  int(11) NULL DEFAULT NULL COMMENT '产品规格id' AFTER `product_unit`,
ADD COLUMN `product_spec_name`  varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NULL DEFAULT NULL COMMENT '产品规格名称' AFTER `product_spec_id`;

ALTER TABLE `revision_storage_detail`
MODIFY COLUMN `revise_product_number`  decimal(10,3) NOT NULL COMMENT '实际数量' AFTER `product_name`,
MODIFY COLUMN `revise_amount`  decimal(10,3) NULL DEFAULT NULL COMMENT '盘库修改数量' AFTER `revise_product_number`,
MODIFY COLUMN `current_product_number`  decimal(10,3) NOT NULL COMMENT '当前数量' AFTER `revise_amount`;

ALTER TABLE `revision_storage_detail`
ADD COLUMN `product_spec_id`  int(11) NULL DEFAULT NULL COMMENT '产品规格id' AFTER `product_unit`,
ADD COLUMN `product_spec_name`  varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NULL DEFAULT NULL COMMENT '产品规格名称' AFTER `product_spec_id`;

ALTER TABLE `free_storage_detail`
ADD COLUMN `product_spec_id`  int(11) NULL DEFAULT NULL COMMENT '产品规格id' AFTER `product_unit`,
ADD COLUMN `product_spec_name`  varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NULL DEFAULT NULL COMMENT '产品规格名称' AFTER `product_spec_id`;

ALTER TABLE `pick_out_storage_detail`
ADD COLUMN `product_spec_id`  int(11) NULL DEFAULT NULL COMMENT '产品规格id' AFTER `product_number`,
ADD COLUMN `product_spec_name`  varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NULL DEFAULT NULL COMMENT '产品规格名称' AFTER `product_spec_id`;

ALTER TABLE `production_out_storage_detail`
ADD COLUMN `product_spec_id`  int(11) NULL DEFAULT NULL COMMENT '产品规格id' AFTER `product_unit`,
ADD COLUMN `product_spec_name`  varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NULL DEFAULT NULL COMMENT '产品规格名称' AFTER `product_spec_id`;


ALTER TABLE `si_cloudmanufacture`.`buy_weight_machine`
ADD COLUMN `data_source` tinyint(4) NULL DEFAULT NULL COMMENT '信息来源 0 物流推送 1 新增称重' AFTER `remark`;

ALTER TABLE `si_cloudmanufacture`.`sale_weight_machine`
ADD COLUMN `data_source` tinyint(4) NULL DEFAULT NULL COMMENT '信息来源 0 物流推送 1 新增称重' AFTER `remark`;

ALTER TABLE `si_cloudmanufacture`.`pro_bagging`
ADD COLUMN `product_spec_id` int(10) NULL DEFAULT NULL COMMENT '产品规格id' AFTER `modify_time`,
ADD COLUMN `product_spec_name` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NULL DEFAULT NULL COMMENT '产品规格名称' AFTER `product_spec_id`;


ALTER TABLE `si_cloudmanufacture`.`buy_weight_machine`
ADD COLUMN `carrier_id` int(10) NULL DEFAULT NULL COMMENT '承运方id' AFTER `data_source`,
ADD COLUMN `delivery_company_id` int(10) NULL DEFAULT NULL COMMENT '发货单位id' AFTER `carrier_id`,
ADD COLUMN `receiving_company_id` int(10) NULL DEFAULT NULL COMMENT '收货单位id' AFTER `delivery_company_id`;


ALTER TABLE `si_cloudmanufacture`.`sale_weight_machine`
ADD COLUMN `carrier_id` int(10) NULL DEFAULT NULL COMMENT '承运方id' AFTER `data_source`,
ADD COLUMN `delivery_company_id` int(10) NULL DEFAULT NULL COMMENT '发货单位id' AFTER `carrier_id`,
ADD COLUMN `receiving_company_id` int(10) NULL DEFAULT NULL COMMENT '收货单位id' AFTER `delivery_company_id`;


ALTER TABLE `si_cloudmanufacture`.`pro_bagging`
ADD COLUMN `step` tinyint(2) NULL AFTER `product_spec_name`;