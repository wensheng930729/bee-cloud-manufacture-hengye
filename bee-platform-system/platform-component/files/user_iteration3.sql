
-- 迭代3数据库脚本

-- 添加承运商类别字段
ALTER TABLE `user_cloudmanufacture`.`auth_customer_or_supplier`
ADD COLUMN `carrier_category` tinyint(1) NULL COMMENT '承运商类别（0公司 1个人）' AFTER `carrier`;

-- 处理历史数据承运商类别字段值
UPDATE auth_customer_or_supplier set carrier_category=0 WHERE carrier=1 AND deleted=0 AND type=1 AND id not in (123,136)
UPDATE auth_customer_or_supplier set carrier_category=1 WHERE id in (123,136)