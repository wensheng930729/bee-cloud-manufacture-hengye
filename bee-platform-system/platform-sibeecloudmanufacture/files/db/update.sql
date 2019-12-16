-- 库存相关的增加企业id和工厂id
ALTER TABLE `buy_product_pending_storage`
ADD COLUMN `factory_id`  int NULL COMMENT '工厂id' AFTER `id`,
ADD COLUMN `org_id`  int NULL COMMENT '企业id' AFTER `factory_id`;

ALTER TABLE `storage_inventory`
ADD COLUMN `factory_id`  int(11) NULL DEFAULT NULL COMMENT '工厂id' AFTER `id`,
ADD COLUMN `org_id`  int(11) NULL DEFAULT NULL COMMENT '企业id' AFTER `factory_id`;

ALTER TABLE `free_storage_detail`
ADD COLUMN `factory_id`  int(11) NULL DEFAULT NULL COMMENT '工厂id' AFTER `free_storage_detail_id`,
ADD COLUMN `org_id`  int(11) NULL DEFAULT NULL COMMENT '企业id' AFTER `factory_id`;

ALTER TABLE `finished_product_pending_storage`
ADD COLUMN `factory_id`  int(11) NULL DEFAULT NULL COMMENT '工厂id' AFTER `finished_product_pending_storage_id`,
ADD COLUMN `org_id`  int(11) NULL DEFAULT NULL COMMENT '企业id' AFTER `factory_id`;

ALTER TABLE `finished_product_be_out_of_storage`
ADD COLUMN `factory_id`  int(11) NULL DEFAULT NULL COMMENT '工厂id' AFTER `contract_car_id`,
ADD COLUMN `org_id`  int(11) NULL DEFAULT NULL COMMENT '企业id' AFTER `factory_id`;

ALTER TABLE `finished_product_out_storage_detail`
ADD COLUMN `factory_id`  int(11) NULL DEFAULT NULL COMMENT '工厂id' AFTER `contract_car_id`,
ADD COLUMN `org_id`  int(11) NULL DEFAULT NULL COMMENT '企业id' AFTER `factory_id`;

ALTER TABLE `pick_out_storage_detail`
ADD COLUMN `factory_id`  int(11) NULL DEFAULT NULL COMMENT '工厂id' AFTER `id`,
ADD COLUMN `org_id`  int(11) NULL DEFAULT NULL COMMENT '企业id' AFTER `factory_id`;

ALTER TABLE `production_out_storage_detail`
ADD COLUMN `factory_id`  int(11) NULL DEFAULT NULL COMMENT '工厂id' AFTER `id`,
ADD COLUMN `org_id`  int(11) NULL DEFAULT NULL COMMENT '企业id' AFTER `factory_id`;

ALTER TABLE `revision_storage_detail`
ADD COLUMN `org_id`  int(11) NULL DEFAULT NULL COMMENT '企业id' AFTER `product_id`,
ADD COLUMN `factory_id`  int(11) NULL DEFAULT NULL COMMENT '工厂id' AFTER `org_id`;