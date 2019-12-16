DROP TABLE IF EXISTS `app`;
CREATE TABLE `app` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT COMMENT 'ID',
  `logo` varchar(255) DEFAULT NULL COMMENT '产品logo',
  `secret` varchar(255) DEFAULT NULL,
  `name` varchar(32) DEFAULT NULL COMMENT '产品名称',
  `abbreviation` varchar(32) DEFAULT NULL COMMENT '产品字母缩写',
  `uri` varchar(255) DEFAULT NULL COMMENT '产品链接',
  `introduction` varchar(255) DEFAULT NULL COMMENT '产品简介',
  `status` int(2) DEFAULT NULL COMMENT '数据状态0删除1正常',
  `remark` varchar(128) CHARACTER SET utf8mb4 DEFAULT '' COMMENT '其他信息',
  PRIMARY KEY (`id`),
  UNIQUE KEY `app_id_index` (`id`) USING BTREE COMMENT '产品信息id索引'
) ENGINE=InnoDB AUTO_INCREMENT=16 DEFAULT CHARSET=utf8 COMMENT='产品信息表';

-- ----------------------------
-- Records of app
-- ----------------------------
INSERT INTO `app` VALUES ('1', null, null, '领蜂供应链', 'SCF', null, '依托多维度数据，形成针对企业完善的信用评估和风险量化体系，在委托采购、委托销售和金融仓储等产融结合的供应链服务中，全面赋能成员企业。', '1', '');
INSERT INTO `app` VALUES ('2', null, null, '蜂创物联', 'IOT', null, '面向工业领域，为企业提供设备接入、数据分析、能耗管理、协同生产等完善的物联网解决方案，助推企业释放数据潜能、提高生产效率。', '1', '');
INSERT INTO `app` VALUES ('3', null, null, '集蜂联运', 'WL', null, '以工业物流数字化服务为基础，整合核心仓储资源，匹配专业运输路径，全面提升物流过程的管理能力。', '1', '');
INSERT INTO `app` VALUES ('4', null, null, '线上蜂贸', 'TRD', null, '基于在线工业贸易解决方案，全面实现企业间的采购、销售数字化，无缝对接内部管理系统，打造智慧化供应链协同平台。', '1', '');
INSERT INTO `app` VALUES ('5', null, null, '金蜜ERP', 'ERP', null, '基于行业定制化的SaaS云产品，致力于为中小企业打造定制化企业管理软件。', '1', '');


DROP TABLE IF EXISTS `app_roles`;
CREATE TABLE `app_roles` (
  `id` bigint(11) NOT NULL AUTO_INCREMENT COMMENT 'ID',
  `app_id` bigint(11) NOT NULL COMMENT '产品id',
  `role_name` varchar(50) DEFAULT NULL COMMENT '角色名称',
  `url` varchar(255) DEFAULT NULL COMMENT '角色对应链接',
  `status` int(2) DEFAULT NULL COMMENT '数据状态0删除1正常',
  `remark` varchar(128) DEFAULT '' COMMENT '其他信息',
  PRIMARY KEY (`id`),
  UNIQUE KEY `app_roles_id_index` (`id`) USING BTREE COMMENT '产品角色信息id索引',
  KEY `app_id_index` (`app_id`) USING BTREE
) ENGINE=InnoDB AUTO_INCREMENT=24 DEFAULT CHARSET=utf8mb4 COMMENT='产品角色信息表';

-- ----------------------------
-- Records of app_roles
-- ----------------------------
INSERT INTO `app_roles` VALUES ('1', '2', '企业管理员', null, '1', '');
INSERT INTO `app_roles` VALUES ('2', '2', '工厂管理员', null, '1', '');
INSERT INTO `app_roles` VALUES ('3', '2', '工厂用户', null, '1', '');
INSERT INTO `app_roles` VALUES ('4', '4', '企业管理员', null, '1', '');
INSERT INTO `app_roles` VALUES ('5', '4', '采购商', null, '1', '');
INSERT INTO `app_roles` VALUES ('6', '4', '供应商', null, '1', '');
INSERT INTO `app_roles` VALUES ('7', '3', '企业管理员', null, '1', '');
INSERT INTO `app_roles` VALUES ('8', '3', '一般用户', null, '1', '');
INSERT INTO `app_roles` VALUES ('9', '3', '企业管理员（物流行业）', null, '1', '');
INSERT INTO `app_roles` VALUES ('10', '3', '一般用户（物流行业）', null, '1', '');
INSERT INTO `app_roles` VALUES ('11', '3', '司机（物流行业）', null, '1', '');


-- ----------------------------
-- Table structure for enterprises_check
-- ----------------------------
ALTER TABLE `enterprises_check`
MODIFY COLUMN `id`  int(10) UNSIGNED NOT NULL AUTO_INCREMENT COMMENT 'id' FIRST ,
MODIFY COLUMN `name`  varchar(32) CHARACTER SET utf8 COLLATE utf8_general_ci NULL DEFAULT NULL COMMENT '待审核企业的名称' AFTER `id`,
MODIFY COLUMN `contact`  varchar(13) CHARACTER SET utf8 COLLATE utf8_general_ci NULL DEFAULT NULL COMMENT '公司联系方式' AFTER `name`,
MODIFY COLUMN `licence`  varchar(32) CHARACTER SET utf8 COLLATE utf8_general_ci NULL DEFAULT NULL COMMENT '企业的执照号码' AFTER `contact`,
MODIFY COLUMN `enclosure`  varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci NULL DEFAULT NULL COMMENT '企业的执照附件' AFTER `licence`,
MODIFY COLUMN `address`  varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci NULL DEFAULT NULL COMMENT '企业的地址' AFTER `enclosure`,
MODIFY COLUMN `admin`  varchar(32) CHARACTER SET utf8 COLLATE utf8_general_ci NULL DEFAULT NULL COMMENT '企业的管理员' AFTER `address`,
MODIFY COLUMN `type`  int(10) UNSIGNED NULL DEFAULT NULL COMMENT '待审核企业的状态(0----未通过|认证 1----通过|认证 2----未审核|认证 3----未通过|修改 4----通过|修改 5----未审核|修改)' AFTER `admin`,
MODIFY COLUMN `check_id`  int(10) UNSIGNED NULL DEFAULT NULL COMMENT '审核员id' AFTER `type`,
MODIFY COLUMN `create_at`  datetime NULL DEFAULT NULL COMMENT '创建日期' AFTER `check_id`,
MODIFY COLUMN `update_at`  datetime NULL DEFAULT NULL COMMENT '更新日期' AFTER `create_at`,
MODIFY COLUMN `real_id`  int(10) UNSIGNED NULL DEFAULT NULL COMMENT '企业在企业表中的真实id' AFTER `update_at`,
ADD COLUMN `create_id`  int(10) NULL DEFAULT NULL COMMENT '创建人id' AFTER `check_id`,
ADD COLUMN `creator`  varchar(50) NULL DEFAULT NULL COMMENT '创建人名称' AFTER `create_id`,
ADD COLUMN `modify_id`  int(10) NULL DEFAULT NULL COMMENT '修改人id' AFTER `create_at`,
ADD COLUMN `modifier`  varchar(50) NULL DEFAULT NULL COMMENT '修改人名称' AFTER `modify_id`,
ADD COLUMN `status`  tinyint(1) NULL DEFAULT 1 COMMENT '数据状态（0删除 1正常）' AFTER `failure_reason`,
COMMENT='企业审核表';
-- -----------------------------
-- 地区表添加索引
-- -----------------------------
ALTER TABLE `common_region`
ADD INDEX `index_pid` (`pid`) ,
ADD INDEX `index_level` (`level`) ;
-- ---------------------------------
-- 企业审核日志表
-- ---------------------------------
-- ----------------------------
-- Table structure for enterprises_check_log
-- ----------------------------
DROP TABLE IF EXISTS `enterprises_check_log`;
CREATE TABLE `enterprises_check_log` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT COMMENT 'id',
  `enterprise_check_id` int(10) NOT NULL COMMENT '企业审核表id',
  `enterprise_name` varchar(32) DEFAULT NULL COMMENT '企业名称',
  `operate_id` int(10) DEFAULT NULL COMMENT '操作人id',
  `operate_name` varchar(255) DEFAULT NULL COMMENT '操作人名称',
  `operate_type` tinyint(2) DEFAULT NULL COMMENT '操作类型(0审核申请，1申请入住)',
  `operate_result` tinyint(2) DEFAULT NULL COMMENT '执行结果（0未通过，1已通过，2未审核）',
  `operate_time` datetime DEFAULT NULL COMMENT '操作时间',
  `refuse_reason` varchar(255) DEFAULT NULL COMMENT '拒绝原因',
  `create_id` int(10) DEFAULT NULL COMMENT '创建人id',
  `creator` varchar(50) DEFAULT NULL COMMENT '创建人名称',
  `create_time` datetime DEFAULT NULL COMMENT '创建时间',
  `modify_id` int(10) DEFAULT NULL COMMENT '修改人id',
  `modifier` varchar(50) DEFAULT NULL COMMENT '修改人名称',
  `modify_time` datetime DEFAULT NULL COMMENT '修改时间',
  `status` tinyint(1) DEFAULT '1' COMMENT '数据状态(0无效，1有效)',
  PRIMARY KEY (`id`),
  KEY `index_enterprise_check_id` (`enterprise_check_id`),
  KEY `index_operate_type` (`operate_type`),
  KEY `index_operate_result` (`operate_result`)
) ENGINE=InnoDB AUTO_INCREMENT=37 DEFAULT CHARSET=utf8mb4 COMMENT='企业审核日志表';

-- 企业附件表

-- ----------------------------
-- Table structure for enterprises_attachment
-- ----------------------------
DROP TABLE IF EXISTS `enterprises_attachment`;
CREATE TABLE `enterprises_attachment`  (
  `id` bigint(20) NOT NULL AUTO_INCREMENT COMMENT 'ID',
  `enterprises_id` int(10) NULL DEFAULT NULL COMMENT '企业id',
  `type` tinyint(1) NULL DEFAULT NULL COMMENT '附件类型（0营业执照 1营业许可证 2企业认证授权书 3logo）',
  `file_name` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NULL DEFAULT NULL COMMENT '附件名称',
  `file_url` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NULL DEFAULT NULL COMMENT '附件url',
  `status` tinyint(1) NULL DEFAULT 1 COMMENT '状态（0无效 1有效）',
  `create_id` int(10) NULL DEFAULT NULL COMMENT '创建人id',
  `creator` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NULL DEFAULT NULL COMMENT '创建人',
  `create_time` datetime(0) NULL DEFAULT NULL COMMENT '创建时间',
  `modify_id` int(10) NULL DEFAULT NULL COMMENT '更新人id',
  `modifier` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NULL DEFAULT NULL COMMENT '更新人',
  `modify_time` datetime(0) NULL DEFAULT NULL COMMENT '更新时间',
  PRIMARY KEY (`id`) USING BTREE,
  INDEX `index_type`(`type`) USING BTREE,
  INDEX `index_status`(`status`) USING BTREE,
  INDEX `index_enterprises_id`(`enterprises_id`) USING BTREE
) ENGINE = InnoDB AUTO_INCREMENT = 98 CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '企业附件信息表' ROW_FORMAT = Dynamic;

-- 企业审核附件表

-- ----------------------------
-- Table structure for enterprises_check_attachment
-- ----------------------------
DROP TABLE IF EXISTS `enterprises_check_attachment`;
CREATE TABLE `enterprises_check_attachment`  (
  `id` bigint(20) NOT NULL AUTO_INCREMENT COMMENT 'ID',
  `enterprises_check_id` int(10) NULL DEFAULT NULL COMMENT '企业申请id',
  `type` tinyint(1) NULL DEFAULT NULL COMMENT '附件类型（0营业执照 1营业许可证 2企业认证授权书 3logo）',
  `file_name` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NULL DEFAULT NULL COMMENT '附件名称',
  `file_url` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NULL DEFAULT NULL COMMENT '附件url',
  `status` tinyint(1) NULL DEFAULT 1 COMMENT '状态（0无效 1有效）',
  `create_id` int(10) NULL DEFAULT NULL COMMENT '创建人id',
  `creator` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NULL DEFAULT NULL COMMENT '创建人',
  `create_time` datetime(0) NULL DEFAULT NULL COMMENT '创建时间',
  `modify_id` int(10) NULL DEFAULT NULL COMMENT '更新人id',
  `modifier` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NULL DEFAULT NULL COMMENT '更新人',
  `modify_time` datetime(0) NULL DEFAULT NULL COMMENT '更新时间',
  PRIMARY KEY (`id`) USING BTREE,
  INDEX `index_enterprises_check_id`(`enterprises_check_id`) USING BTREE,
  INDEX `index_type`(`type`) USING BTREE,
  INDEX `index_status`(`status`) USING BTREE
) ENGINE = InnoDB AUTO_INCREMENT = 287 CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '企业附件信息表' ROW_FORMAT = Dynamic;

-- 企业关联用户审核表
-- ----------------------------
-- Table structure for enterprises_relation_user_check
-- ----------------------------
DROP TABLE IF EXISTS `enterprises_relation_user_check`;
CREATE TABLE `enterprises_relation_user_check`  (
  `id` int(10) UNSIGNED NOT NULL AUTO_INCREMENT COMMENT 'id',
  `enterprise_id` int(10) UNSIGNED NULL DEFAULT NULL COMMENT '企业id',
  `enterprise_name` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NULL DEFAULT NULL COMMENT '企业名称',
  `user_id` int(10) UNSIGNED NULL DEFAULT NULL COMMENT '用户id',
  `user_name` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NULL DEFAULT NULL COMMENT '用户名',
  `phone` varchar(11) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NULL DEFAULT NULL COMMENT '手机号',
  `check_id` int(10) NULL DEFAULT NULL COMMENT '审核人id',
  `check_name` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NULL DEFAULT NULL COMMENT '审核人姓名',
  `check_status` tinyint(1) NULL DEFAULT NULL COMMENT '审核状态（0未通过 1已通过 2未审核）',
  `apply_reason` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NULL DEFAULT NULL COMMENT '申请理由',
  `refusal_reason` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NULL DEFAULT NULL COMMENT '拒绝原因',
  `status` tinyint(1) NULL DEFAULT 1 COMMENT '数据状态（0删除 1正常）',
  `create_id` int(10) NULL DEFAULT NULL COMMENT '创建人id',
  `creator` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NULL DEFAULT NULL COMMENT '创建人名称',
  `create_time` datetime(0) NULL DEFAULT NULL COMMENT '创建时间',
  `modify_id` int(10) NULL DEFAULT NULL COMMENT '更新人id',
  `modifier` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NULL DEFAULT NULL COMMENT '更新人',
  `modify_time` datetime(0) NULL DEFAULT NULL COMMENT '更新时间',
  PRIMARY KEY (`id`) USING BTREE,
  INDEX `index_user_id`(`user_id`) USING BTREE,
  INDEX `index_check_status`(`check_status`) USING BTREE
) ENGINE = InnoDB AUTO_INCREMENT = 24 CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '企业关联用户审核表' ROW_FORMAT = Dynamic;

-- 企业关联用户审核日志表
-- ----------------------------
-- Table structure for enterprises_relation_user_check_log
-- ----------------------------
DROP TABLE IF EXISTS `enterprises_relation_user_check_log`;
CREATE TABLE `enterprises_relation_user_check_log`  (
  `id` bigint(20) NOT NULL AUTO_INCREMENT COMMENT 'id',
  `enterprise_relation_user_check_id` int(10) NOT NULL COMMENT '企业关联用户审核表id',
  `enterprise_id` int(10) NULL DEFAULT NULL COMMENT '企业id',
  `enterprise_name` varchar(32) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NULL DEFAULT NULL COMMENT '企业名称',
  `operate_id` int(10) NULL DEFAULT NULL COMMENT '操作人id',
  `operate_name` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NULL DEFAULT NULL COMMENT '操作人名称',
  `operate_type` tinyint(2) NULL DEFAULT NULL COMMENT '操作类型(0审核申请，1申请关联)',
  `operate_result` tinyint(2) NULL DEFAULT NULL COMMENT '执行结果（0未通过，1已通过，2未审核）',
  `operate_time` datetime(0) NULL DEFAULT NULL COMMENT '操作时间',
  `refuse_reason` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NULL DEFAULT NULL COMMENT '拒绝原因',
  `create_id` int(10) NULL DEFAULT NULL COMMENT '创建人id',
  `creator` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NULL DEFAULT NULL COMMENT '创建人名称',
  `create_time` datetime(0) NULL DEFAULT NULL COMMENT '创建时间',
  `modify_id` int(10) NULL DEFAULT NULL COMMENT '修改人id',
  `modifier` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NULL DEFAULT NULL COMMENT '修改人名称',
  `modify_time` datetime(0) NULL DEFAULT NULL COMMENT '修改时间',
  `status` tinyint(1) NULL DEFAULT 1 COMMENT '数据状态(0无效，1有效)',
  PRIMARY KEY (`id`) USING BTREE,
  INDEX `index_enterprise_relation_user_check_id`(`enterprise_relation_user_check_id`) USING BTREE,
  INDEX `index_operate_type`(`operate_type`) USING BTREE,
  INDEX `index_operate_result`(`operate_result`) USING BTREE
) ENGINE = InnoDB AUTO_INCREMENT = 28 CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '企业关联用户审核日志表' ROW_FORMAT = Dynamic;


-- ----------------------------
-- Table structure for z_post
-- ----------------------------
DROP TABLE IF EXISTS `z_post`;
CREATE TABLE `z_post`  (
  `id` int(20) NOT NULL AUTO_INCREMENT COMMENT '职位ID',
  `name` varchar(20) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci DEFAULT NULL COMMENT '职位名称',
  `department_id` int(20) DEFAULT NULL COMMENT '部门ID ',
  `status` tinyint(4) DEFAULT NULL COMMENT '状态：0删除',
  `description` varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '职位描述',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB AUTO_INCREMENT = 21 CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '职位表' ROW_FORMAT = Dynamic;

-- 中台系统通知表
-- ----------------------------
-- Table structure for middle_system_notice
-- ----------------------------
DROP TABLE IF EXISTS `middle_system_notice`;
CREATE TABLE `middle_system_notice`  (
  `id` bigint(20) NOT NULL AUTO_INCREMENT COMMENT 'id',
  `notifier_id` int(10) NOT NULL COMMENT '通知人id',
  `title` varchar(60) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL COMMENT '通知标题',
  `content` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NULL DEFAULT NULL COMMENT '通知内容',
  `is_read` tinyint(1) NOT NULL DEFAULT 0 COMMENT '是否阅读，0未读，1已读',
  `status` tinyint(1) NOT NULL DEFAULT 1 COMMENT '是否有效，0无效，1有效',
  `create_time` datetime(0) NOT NULL COMMENT '创建时间',
  `update_time` datetime(0) NOT NULL COMMENT '修改时间',
  PRIMARY KEY (`id`) USING BTREE,
  INDEX `notifiter_id`(`notifier_id`) USING BTREE COMMENT '通知人id索引'
) ENGINE = InnoDB AUTO_INCREMENT = 520 CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '中台系统通知' ROW_FORMAT = Dynamic;

-- 行业分类信息表
-- ----------------------------
-- Table structure for t_industry
-- ----------------------------
DROP TABLE IF EXISTS `t_industry`;
CREATE TABLE `t_industry`  (
  `id` int(10) UNSIGNED NOT NULL AUTO_INCREMENT COMMENT 'id',
  `pid` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '父级行业关系',
  `industry` varchar(120) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '行业名称',
  `level` tinyint(2) NOT NULL COMMENT '子属级别关系',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB AUTO_INCREMENT = 17 CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '行业分类信息表' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of t_industry
-- ----------------------------
INSERT INTO `t_industry` VALUES (1, 13, '办公文教', 2);
INSERT INTO `t_industry` VALUES (2, 13, '电工电气', 2);
INSERT INTO `t_industry` VALUES (3, 13, '五金工具', 2);
INSERT INTO `t_industry` VALUES (4, 13, '化工', 2);
INSERT INTO `t_industry` VALUES (5, 13, '橡塑', 2);
INSERT INTO `t_industry` VALUES (6, 13, '环保', 2);
INSERT INTO `t_industry` VALUES (7, 13, '能源', 2);
INSERT INTO `t_industry` VALUES (8, 13, '冶金矿产', 2);
INSERT INTO `t_industry` VALUES (9, 13, '钢铁', 2);
INSERT INTO `t_industry` VALUES (10, 14, '运输', 2);
INSERT INTO `t_industry` VALUES (11, 14, '仓储', 2);
INSERT INTO `t_industry` VALUES (12, 14, '综合物流', 2);
INSERT INTO `t_industry` VALUES (13, 0, '一般企业', 1);
INSERT INTO `t_industry` VALUES (14, 0, '物流企业', 1);


-- enterprises增加一些字段
ALTER TABLE `enterprises`
ADD COLUMN `admin`  varchar(32) CHARACTER SET utf8 COLLATE utf8_general_ci NULL DEFAULT NULL COMMENT '企业管理员' AFTER `head`,
MODIFY COLUMN `industry`  varchar(10) CHARACTER SET utf8 COLLATE utf8_general_ci NULL DEFAULT NULL COMMENT '企业所属行业' AFTER `contact`,
ADD COLUMN `linkman`  varchar(20) CHARACTER SET utf8 COLLATE utf8_general_ci NULL DEFAULT NULL COMMENT '指定联系人' AFTER `industry`,
ADD COLUMN `street`  varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci NULL DEFAULT NULL COMMENT '详细街道地址' AFTER `linkman`,
ADD COLUMN `regionid`  varchar(6) CHARACTER SET utf8 COLLATE utf8_general_ci NULL DEFAULT NULL COMMENT '县级地区id' AFTER `street`,
MODIFY COLUMN `address`  varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci NULL DEFAULT NULL COMMENT '地址' AFTER `regionid`,
ADD COLUMN `status`  tinyint(1) NULL DEFAULT 1 COMMENT '数据状态（0删除 1正常）' AFTER `address`,
MODIFY COLUMN `type`  int(2) NULL DEFAULT NULL COMMENT '企业类型 1企业 2物流商' AFTER `status`,
MODIFY COLUMN `create_at`  datetime NULL DEFAULT NULL COMMENT '认证通过时间' AFTER `enclosure`,
MODIFY COLUMN `update_at`  datetime NULL DEFAULT NULL COMMENT '修改时间' AFTER `create_at`;

-- ---------------------------------
-- 删除企业名称重复没有关联过用户的的企业
-- ---------------------------------

UPDATE enterprises
SET `status` = 0
WHERE
	id IN (
SELECT
	newID
FROM
	(
SELECT
	*
FROM
	`enterprises_users` u
	RIGHT JOIN ( SELECT id AS newID FROM enterprises WHERE `name` IN ( SELECT `name` FROM `enterprises` GROUP BY NAME HAVING count( NAME ) > 1 ) ) e ON u.enterprise_id = e.newID
WHERE
	u.user_id IS NULL
	) b
	);


-- ------------------------------------
-- index_init增加字段
-- ------------------------------------
ALTER TABLE `index_init`
MODIFY COLUMN `register_enterprise`  int(11) NOT NULL COMMENT '首页-入驻企业' FIRST ,
MODIFY COLUMN `public_requirement`  int(11) NOT NULL COMMENT '集峰联运-发布量（个）' AFTER `register_enterprise`,
MODIFY COLUMN `supply_money`  int(11) NOT NULL COMMENT '首页-成交金额' AFTER `public_requirement`,
CHANGE COLUMN `Logistical_enterprise_cnt` `logistical_enterprise_cnt`  int(11) NOT NULL COMMENT '集峰联运-合作商家' AFTER `chain_service_cnt`,
MODIFY COLUMN `logistical_service_cnt`  int(11) NOT NULL COMMENT '集峰联运-单量' AFTER `logistical_enterprise_cnt`,
MODIFY COLUMN `number8`  int(11) NOT NULL COMMENT '首页-完成订单数量' AFTER `logistical_service_cnt`,
MODIFY COLUMN `number9`  int(11) NOT NULL COMMENT '集峰联运-运量' AFTER `number8`;


ALTER TABLE `index_init`
CHANGE COLUMN `number8` `finished_order_cnt`  int(11) NOT NULL COMMENT '首页-完成订单数量' AFTER `logistical_service_cnt`,
CHANGE COLUMN `number9` `transport_volume`  int(11) NOT NULL COMMENT '集峰联运-运量' AFTER `finished_order_cnt`;


ALTER TABLE `index_init`
ADD COLUMN `count_company`  int(11) NULL COMMENT '线上峰贸-认证供应商(家)' AFTER `number10`,
ADD COLUMN `count_vaild_inquiry`  int(11) NULL COMMENT '线上峰贸-最新采购询价(单)' AFTER `count_company`,
ADD COLUMN `count_vaild_quoted`  int(11) NULL COMMENT '线上峰贸-报价中的供应商(家)' AFTER `count_vaild_inquiry`,
ADD COLUMN `count_money`  int(11) NULL COMMENT '线上峰贸-成交总额' AFTER `count_vaild_quoted`;


ALTER TABLE `index_init`
MODIFY COLUMN `supply_money`  bigint(11) NOT NULL COMMENT '首页-成交金额' AFTER `public_requirement`,
MODIFY COLUMN `count_money`  bigint(11) NULL DEFAULT NULL COMMENT '线上峰贸-成交总额' AFTER `count_vaild_quoted`;


-- ------------------------------------
-- 中台新增字段
-- ------------------------------------
ALTER TABLE `st_platform`.`users`
ADD COLUMN `update_id` int(10) COMMENT '修改人id' AFTER `fixtel`;


INSERT INTO `st_platform`.`t_system_code_t`(`sys_group_id`, `sys_code`, `sys_code_val`, `sys_code_desc`, `status`, `order_num`, `create_time`, `update_time`) VALUES ('platform_permission_client_id', 'platform_reception', 'com.bee.platform.common.service.impl.AuthServiceImpl', '前中台拦截器权限校验实现类', '1', NULL, NULL, '2019-05-16 10:57:51');
INSERT INTO `st_platform`.`t_system_code_t`(`sys_group_id`, `sys_code`, `sys_code_val`, `sys_code_desc`, `status`, `order_num`, `create_time`, `update_time`) VALUES ('platform_permission_client_id', 'platform_backSystem', 'com.bee.platform.common.service.impl.BackAuthServiceImpl', '后台拦截器权限校验实现类', '1', NULL, NULL, '2019-05-16 10:57:44');


INSERT INTO `st_platform`.`t_config`(`config_key`, `config_value`, `config_desc`, `status`, `create_time`, `update_time`) VALUES ('default_password', '123456', '新增用户时的默认密码，默认123456配置表中可配置', '1', '2019-05-14 16:16:54', '2019-05-14 16:16:54');


-- ----------------------------
-- users
-- ----------------------------
alter table `st_platform`.`users` add COLUMN `consumer_name` varchar(32) DEFAULT NULL COMMENT '用户名' after `phone`;
alter table `st_platform`.`users` add COLUMN `beesrv_id` varchar(50) DEFAULT NULL COMMENT '业务id' after `UUID`;

ALTER TABLE `st_platform`.`enterprises_users`
ADD COLUMN `role_id` int(10) COMMENT '角色id' AFTER `app_ids`;

ALTER TABLE `st_platform`.`enterprises_users`
ADD COLUMN `status` tinyint(1) COMMENT '状态' AFTER `role_id`;

ALTER TABLE `st_platform`.`enterprises_users`
ADD COLUMN `create_id` int(10) COMMENT '创建人id' AFTER `status`;

ALTER TABLE `st_platform`.`enterprises_users`
ADD COLUMN `create_at` datetime COMMENT '创建时间' AFTER `create_id`;

ALTER TABLE `st_platform`.`enterprises_users`
ADD COLUMN `modify_id` int(10) COMMENT '修改人id' AFTER `create_at`;

ALTER TABLE `st_platform`.`enterprises_users`
ADD COLUMN `modify_at` datetime COMMENT '修改时间' AFTER `modify_id`;

ALTER TABLE `st_platform`.`enterprises_users`
ADD COLUMN `phone` varchar(20) COMMENT '用户电话' AFTER `modify_at`;

ALTER TABLE `st_platform`.`enterprises_users`
ADD COLUMN `departmentsid` int(10) COMMENT '部门ID' AFTER `phone`;

ALTER TABLE `st_platform`.`enterprises_users`
ADD COLUMN `email` varchar(255) COMMENT '企业用户邮箱' AFTER `departmentsid`;

ALTER TABLE `st_platform`.`enterprises_users`
ADD COLUMN `fixtel` varchar(20) COMMENT '固话' AFTER `email`;

ALTER TABLE `st_platform`.`enterprises_users`
ADD COLUMN `regionid` varchar(5) COMMENT '县级地区id' AFTER `fixtel`;

ALTER TABLE `st_platform`.`enterprises_users`
ADD COLUMN `address` varchar(20) COMMENT '详细地址' AFTER `regionid`;

ALTER TABLE `st_platform`.`enterprises_users`
ADD COLUMN `zpostid` int(10) COMMENT '详细地址' AFTER `address`;

ALTER TABLE `st_platform`.`users_departments`
ADD COLUMN `post_id` int(10) COMMENT '职位id' AFTER `department_id`;


update `st_platform`.`enterprises_users` set status =1;


update `st_platform`.`users_roles` set role_id =1 WHERE role_id =2;

DELETE from  `st_platform`.`u_roles`  WHERE id=2;


-- ----------------------------
-- Table structure for m_managers_roles
-- ----------------------------
DROP TABLE IF EXISTS `m_managers_roles`;
CREATE TABLE `m_managers_roles`  (
  `id` int(10) UNSIGNED NOT NULL AUTO_INCREMENT,
  `manager_id` int(10) UNSIGNED DEFAULT NULL,
  `role_id` int(10) UNSIGNED DEFAULT NULL,
  PRIMARY KEY (`id`) USING BTREE,
  INDEX `roles_with_managers_in_relation_foreign_key`(`role_id`) USING BTREE,
  INDEX `managers_with_roles_in_relation_foreign_key`(`manager_id`) USING BTREE
) ENGINE = InnoDB AUTO_INCREMENT = 13 CHARACTER SET = utf8 COLLATE = utf8_general_ci ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of m_managers_roles
-- ----------------------------
INSERT INTO `m_managers_roles` VALUES (1, 1, 1);

-- ----------------------------
-- Table managers_roles
-- ----------------------------
DROP TABLE IF EXISTS `managers_roles`;
-- ----------------------------
-- Table structure for m_roles
-- ----------------------------
DROP TABLE IF EXISTS `m_roles`;
CREATE TABLE `m_roles`  (
  `role_id` int(10) UNSIGNED NOT NULL AUTO_INCREMENT COMMENT '角色id',
  `role_name` varchar(32) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '角色名称',
  `role_type` tinyint(1) DEFAULT NULL COMMENT '角色类型',
  `create_company_id` varchar(30) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '创建企业',
  `explain` varchar(50) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '角色说明',
  `status` tinyint(1) DEFAULT NULL COMMENT '角色状态',
  `create_id` int(11) DEFAULT NULL COMMENT '创建人id',
  `creator` varchar(60) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '创建人姓名',
  `create_time` datetime(0) DEFAULT NULL COMMENT '创建时间',
  `modify_id` int(11) DEFAULT NULL COMMENT '修改人id',
  `modifier` varchar(60) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '修改人姓名',
  `modify_time` datetime(0) DEFAULT NULL COMMENT '修改时间',
  PRIMARY KEY (`role_id`) USING BTREE
) ENGINE = InnoDB AUTO_INCREMENT = 86 CHARACTER SET = utf8 COLLATE = utf8_general_ci COMMENT = '管理员角色表' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of m_roles
-- ----------------------------
INSERT INTO `m_roles` VALUES (1, '超级管理员', 1, NULL, NULL, 1, 0, 'admin', '2019-04-29 15:56:53', NULL, NULL, '2019-04-29 15:56:53');
INSERT INTO `m_roles` VALUES (2, '首页', 4, NULL, NULL, 1, 1, 'admin', '2019-05-13 16:15:27', NULL, NULL, '2019-05-13 16:15:29');
INSERT INTO `m_roles` VALUES (3, '查看', 2, NULL, '首页的查询', 1, 1, 'admin', '2019-05-13 16:15:53', NULL, NULL, '2019-05-13 16:15:56');
INSERT INTO `m_roles` VALUES (4, '企业管理', 4, NULL, NULL, 1, 0, 'admin', '2019-04-29 15:56:53', NULL, NULL, '2019-04-29 15:56:53');
INSERT INTO `m_roles` VALUES (5, '查看', 2, NULL, '企业管理的查看权限集', 1, 0, 'admin', '2019-04-29 15:56:53', NULL, NULL, '2019-04-29 15:56:53');
INSERT INTO `m_roles` VALUES (6, '编辑', 2, NULL, '企业管理的编辑权限集', 1, 0, 'admin', '2019-04-29 15:56:53', NULL, NULL, '2019-04-29 15:56:53');
INSERT INTO `m_roles` VALUES (7, '产品开通审核', 4, NULL, NULL, 1, 0, 'admin', '2019-04-29 15:56:53', NULL, NULL, '2019-04-29 15:56:53');
INSERT INTO `m_roles` VALUES (8, '查看', 2, NULL, '产品开通审核的查看权限', 1, 0, 'admin', '2019-04-29 15:56:53', NULL, NULL, '2019-04-29 15:56:53');
INSERT INTO `m_roles` VALUES (9, '编辑', 2, NULL, '产品开通审核的编辑权限', 1, 0, 'admin', '2019-04-29 15:56:53', NULL, NULL, '2019-04-29 15:56:53');
INSERT INTO `m_roles` VALUES (10, '用户管理', 4, NULL, NULL, 1, 0, 'admin', '2019-04-29 15:56:53', NULL, NULL, '2019-04-29 15:56:53');
INSERT INTO `m_roles` VALUES (11, '查看', 2, NULL, '用户管理的查看权限', 1, 0, 'admin', '2019-04-29 15:56:53', NULL, NULL, '2019-04-29 15:56:53');
INSERT INTO `m_roles` VALUES (12, '编辑', 2, NULL, '用户管理的编辑权限', 1, 0, 'admin', '2019-04-29 15:56:53', NULL, NULL, '2019-04-29 15:56:53');
INSERT INTO `m_roles` VALUES (13, '前台配置', 4, NULL, NULL, 1, 1, 'admin', '2019-05-13 16:15:53', NULL, NULL, '2019-05-13 16:15:53');
INSERT INTO `m_roles` VALUES (14, '查看', 2, NULL, NULL, 1, 1, 'admin', '2019-05-13 16:15:53', NULL, NULL, '2019-05-13 16:15:53');
INSERT INTO `m_roles` VALUES (15, '编辑', 2, NULL, NULL, 1, 1, 'admin', '2019-05-13 16:15:53', NULL, NULL, '2019-05-13 16:15:53');
INSERT INTO `m_roles` VALUES (16, '工单及意见', 4, NULL, NULL, 1, 1, 'admin', '2019-05-13 16:15:53', NULL, NULL, '2019-05-13 16:15:53');
INSERT INTO `m_roles` VALUES (17, '查看', 2, NULL, NULL, 1, 1, 'admin', '2019-05-13 16:15:53', NULL, NULL, '2019-05-13 16:15:53');
INSERT INTO `m_roles` VALUES (18, '回复', 2, NULL, NULL, 1, 1, 'admin', '2019-05-13 16:15:53', NULL, NULL, '2019-05-13 16:15:53');
INSERT INTO `m_roles` VALUES (19, '数据仓库后台', 4, NULL, NULL, 1, 0, 'admin', '2019-04-29 15:56:53', NULL, NULL, '2019-04-29 15:56:53');
INSERT INTO `m_roles` VALUES (20, '查看', 2, NULL, '数据仓库后台的查看权限', 1, 0, 'admin', '2019-04-29 15:56:53', NULL, NULL, '2019-04-29 15:56:53');
INSERT INTO `m_roles` VALUES (21, '编辑', 2, NULL, '数据仓库后台的编辑权限', 1, 0, 'admin', '2019-04-29 15:56:53', NULL, NULL, '2019-04-29 15:56:53');
INSERT INTO `m_roles` VALUES (22, '资讯管理', 4, NULL, NULL, 1, 0, 'admin', '2019-04-29 15:56:53', NULL, NULL, '2019-04-29 15:56:53');
INSERT INTO `m_roles` VALUES (23, '查看', 2, NULL, '资讯管理的查看权限', 1, 0, 'admin', '2019-04-29 15:56:53', NULL, NULL, '2019-04-29 15:56:53');
INSERT INTO `m_roles` VALUES (24, '编辑', 2, NULL, '资讯管理的编辑权限', 1, 0, 'admin', '2019-04-29 15:56:53', NULL, NULL, '2019-04-29 15:56:53');
INSERT INTO `m_roles` VALUES (25, '系统帮助', 4, NULL, NULL, 1, 1, 'admin', '2019-05-13 16:15:53', NULL, NULL, '2019-05-13 16:15:53');
INSERT INTO `m_roles` VALUES (26, '查看', 2, NULL, NULL, 1, 1, 'admin', '2019-05-13 16:15:53', NULL, NULL, '2019-05-13 16:15:53');
INSERT INTO `m_roles` VALUES (27, '编辑', 2, NULL, NULL, 1, 1, 'admin', '2019-05-13 16:15:53', NULL, NULL, '2019-05-13 16:15:53');
INSERT INTO `m_roles` VALUES (28, '权限-权限配置', 2, NULL, NULL, 1, 1, 'admin', '2019-05-13 16:15:53', NULL, NULL, '2019-05-13 16:15:53');
INSERT INTO `m_roles` VALUES (29, '权限-用户关联', 2, NULL, NULL, 1, 1, 'admin', '2019-05-13 16:15:53', NULL, NULL, '2019-05-13 16:15:53');

-- ----------------------------
-- Table structure for m_role_role
-- ----------------------------
DROP TABLE IF EXISTS `m_role_role`;
CREATE TABLE `m_role_role`  (
  `id` int(11) NOT NULL AUTO_INCREMENT COMMENT '主键',
  `parent_role_id` int(11) DEFAULT NULL COMMENT '父角色id',
  `child_role_id` int(11) DEFAULT NULL COMMENT '子级角色id',
  PRIMARY KEY (`id`) USING BTREE,
  UNIQUE INDEX `combination`(`parent_role_id`, `child_role_id`) USING BTREE
) ENGINE = InnoDB AUTO_INCREMENT = 240 CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '角色与角色之间的关联表' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of m_role_role
-- ----------------------------
INSERT INTO `m_role_role` VALUES (221, 1, 3);
INSERT INTO `m_role_role` VALUES (222, 1, 5);
INSERT INTO `m_role_role` VALUES (223, 1, 6);
INSERT INTO `m_role_role` VALUES (224, 1, 8);
INSERT INTO `m_role_role` VALUES (225, 1, 9);
INSERT INTO `m_role_role` VALUES (226, 1, 11);
INSERT INTO `m_role_role` VALUES (227, 1, 12);
INSERT INTO `m_role_role` VALUES (228, 1, 14);
INSERT INTO `m_role_role` VALUES (229, 1, 15);
INSERT INTO `m_role_role` VALUES (230, 1, 17);
INSERT INTO `m_role_role` VALUES (231, 1, 18);
INSERT INTO `m_role_role` VALUES (232, 1, 20);
INSERT INTO `m_role_role` VALUES (233, 1, 21);
INSERT INTO `m_role_role` VALUES (234, 1, 23);
INSERT INTO `m_role_role` VALUES (235, 1, 24);
INSERT INTO `m_role_role` VALUES (236, 1, 26);
INSERT INTO `m_role_role` VALUES (237, 1, 27);
INSERT INTO `m_role_role` VALUES (238, 1, 28);
INSERT INTO `m_role_role` VALUES (239, 1, 29);
INSERT INTO `m_role_role` VALUES (189, 2, 3);
INSERT INTO `m_role_role` VALUES (190, 4, 5);
INSERT INTO `m_role_role` VALUES (191, 4, 6);
INSERT INTO `m_role_role` VALUES (192, 7, 8);
INSERT INTO `m_role_role` VALUES (193, 7, 9);
INSERT INTO `m_role_role` VALUES (194, 10, 11);
INSERT INTO `m_role_role` VALUES (195, 10, 12);
INSERT INTO `m_role_role` VALUES (196, 13, 14);
INSERT INTO `m_role_role` VALUES (197, 13, 15);
INSERT INTO `m_role_role` VALUES (198, 16, 17);
INSERT INTO `m_role_role` VALUES (199, 16, 18);
INSERT INTO `m_role_role` VALUES (200, 19, 20);
INSERT INTO `m_role_role` VALUES (201, 19, 21);
INSERT INTO `m_role_role` VALUES (202, 22, 23);
INSERT INTO `m_role_role` VALUES (203, 22, 24);
INSERT INTO `m_role_role` VALUES (204, 25, 26);
INSERT INTO `m_role_role` VALUES (205, 25, 27);

-- ----------------------------
-- Table structure for m_role_resource
-- ----------------------------
DROP TABLE IF EXISTS `m_role_resource`;
CREATE TABLE `m_role_resource`  (
  `id` int(11) NOT NULL AUTO_INCREMENT COMMENT '主键',
  `role_id` int(11) DEFAULT NULL COMMENT '角色id',
  `resource_id` int(11) DEFAULT NULL COMMENT '资源id',
  PRIMARY KEY (`id`) USING BTREE,
  UNIQUE INDEX `role_resource`(`role_id`, `resource_id`) USING BTREE,
  INDEX `role_id`(`role_id`) USING BTREE
) ENGINE = InnoDB AUTO_INCREMENT = 91 CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '角色资源关联表' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of m_role_resource
-- ----------------------------
INSERT INTO `m_role_resource` VALUES (1, 1, 37);
INSERT INTO `m_role_resource` VALUES (2, 1, 38);
INSERT INTO `m_role_resource` VALUES (3, 1, 39);
INSERT INTO `m_role_resource` VALUES (4, 1, 40);
INSERT INTO `m_role_resource` VALUES (5, 1, 41);
INSERT INTO `m_role_resource` VALUES (6, 1, 42);
INSERT INTO `m_role_resource` VALUES (7, 1, 43);
INSERT INTO `m_role_resource` VALUES (8, 1, 44);
INSERT INTO `m_role_resource` VALUES (9, 1, 45);
INSERT INTO `m_role_resource` VALUES (10, 1, 46);
INSERT INTO `m_role_resource` VALUES (11, 1, 47);
INSERT INTO `m_role_resource` VALUES (12, 1, 48);
INSERT INTO `m_role_resource` VALUES (13, 1, 49);
INSERT INTO `m_role_resource` VALUES (14, 1, 50);
INSERT INTO `m_role_resource` VALUES (15, 1, 51);
INSERT INTO `m_role_resource` VALUES (16, 1, 52);
INSERT INTO `m_role_resource` VALUES (17, 1, 53);
INSERT INTO `m_role_resource` VALUES (18, 1, 54);
INSERT INTO `m_role_resource` VALUES (19, 1, 55);
INSERT INTO `m_role_resource` VALUES (20, 1, 56);
INSERT INTO `m_role_resource` VALUES (21, 1, 57);
INSERT INTO `m_role_resource` VALUES (22, 1, 58);
INSERT INTO `m_role_resource` VALUES (23, 1, 59);
INSERT INTO `m_role_resource` VALUES (24, 1, 60);
INSERT INTO `m_role_resource` VALUES (25, 1, 61);
INSERT INTO `m_role_resource` VALUES (26, 1, 62);
INSERT INTO `m_role_resource` VALUES (27, 1, 63);
INSERT INTO `m_role_resource` VALUES (28, 1, 64);
INSERT INTO `m_role_resource` VALUES (29, 1, 65);
INSERT INTO `m_role_resource` VALUES (30, 1, 66);
INSERT INTO `m_role_resource` VALUES (31, 1, 67);
INSERT INTO `m_role_resource` VALUES (32, 1, 68);
INSERT INTO `m_role_resource` VALUES (59, 3, 37);
INSERT INTO `m_role_resource` VALUES (60, 3, 38);
INSERT INTO `m_role_resource` VALUES (61, 3, 39);
INSERT INTO `m_role_resource` VALUES (62, 5, 40);
INSERT INTO `m_role_resource` VALUES (63, 5, 41);
INSERT INTO `m_role_resource` VALUES (64, 6, 60);
INSERT INTO `m_role_resource` VALUES (65, 8, 42);
INSERT INTO `m_role_resource` VALUES (66, 9, 61);
INSERT INTO `m_role_resource` VALUES (67, 11, 46);
INSERT INTO `m_role_resource` VALUES (68, 11, 47);
INSERT INTO `m_role_resource` VALUES (69, 12, 62);
INSERT INTO `m_role_resource` VALUES (70, 14, 45);
INSERT INTO `m_role_resource` VALUES (71, 15, 63);
INSERT INTO `m_role_resource` VALUES (72, 17, 48);
INSERT INTO `m_role_resource` VALUES (73, 17, 49);
INSERT INTO `m_role_resource` VALUES (74, 17, 50);
INSERT INTO `m_role_resource` VALUES (75, 18, 64);
INSERT INTO `m_role_resource` VALUES (76, 23, 51);
INSERT INTO `m_role_resource` VALUES (77, 23, 52);
INSERT INTO `m_role_resource` VALUES (78, 24, 65);
INSERT INTO `m_role_resource` VALUES (79, 26, 55);
INSERT INTO `m_role_resource` VALUES (80, 27, 56);
INSERT INTO `m_role_resource` VALUES (81, 27, 57);
INSERT INTO `m_role_resource` VALUES (82, 27, 58);
INSERT INTO `m_role_resource` VALUES (83, 27, 59);
INSERT INTO `m_role_resource` VALUES (84, 27, 66);
INSERT INTO `m_role_resource` VALUES (85, 28, 53);
INSERT INTO `m_role_resource` VALUES (86, 28, 54);
INSERT INTO `m_role_resource` VALUES (87, 28, 67);
INSERT INTO `m_role_resource` VALUES (88, 29, 43);
INSERT INTO `m_role_resource` VALUES (89, 29, 44);
INSERT INTO `m_role_resource` VALUES (90, 29, 68);

-- ----------------------------
-- Table structure for m_resource
-- ----------------------------
DROP TABLE IF EXISTS `m_resource`;
CREATE TABLE `m_resource`  (
  `id` int(11) NOT NULL AUTO_INCREMENT COMMENT '主键',
  `resource_id` int(11) NOT NULL COMMENT '资源id',
  `name` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci DEFAULT NULL COMMENT '资源名称',
  `parent_id` int(11) DEFAULT NULL COMMENT '父级id',
  `resource_type` tinyint(1) DEFAULT NULL COMMENT '资源类型',
  `resource_lev` tinyint(1) DEFAULT NULL COMMENT '资源级别',
  `icon` varchar(120) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci DEFAULT NULL COMMENT '资源图标',
  `path` varchar(120) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci DEFAULT NULL COMMENT '前端给的路径',
  `hide_children_in_menu` tinyint(1) DEFAULT NULL COMMENT '是否显示子菜单0否，1是，前端初始化的时候给',
  `explain` varchar(100) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci DEFAULT NULL COMMENT '资源说明',
  `intercept` tinyint(1) DEFAULT NULL COMMENT '拦截等级',
  `status` tinyint(1) DEFAULT NULL COMMENT '是否有效0无效，1有效',
  `create_id` int(11) DEFAULT NULL COMMENT '创建人id',
  `creator` varchar(60) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci DEFAULT NULL COMMENT '创建人姓名',
  `create_time` datetime(0) DEFAULT NULL COMMENT '创建时间',
  `modify_id` int(11) DEFAULT NULL COMMENT '修改人id',
  `modifier` varchar(60) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci DEFAULT NULL COMMENT '修改人姓名',
  `modify_time` datetime(0) DEFAULT NULL COMMENT '修改时间',
  PRIMARY KEY (`id`) USING BTREE,
  UNIQUE INDEX `resource`(`resource_id`) USING BTREE
) ENGINE = InnoDB AUTO_INCREMENT = 36 CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of m_resource
-- ----------------------------
INSERT INTO `m_resource`(`id`, `resource_id`, `name`, `parent_id`, `resource_type`, `resource_lev`, `icon`, `path`, `hide_children_in_menu`, `explain`, `intercept`, `status`, `create_id`, `creator`, `create_time`, `modify_id`, `modifier`, `modify_time`) VALUES (3, 37, '首页', 0, 1, 1, 'dashboard', '/home', 1, NULL, NULL, 1, 1, 'admin', '2019-05-13 13:27:39', NULL, NULL, '2019-05-13 13:27:39');
INSERT INTO `m_resource`(`id`, `resource_id`, `name`, `parent_id`, `resource_type`, `resource_lev`, `icon`, `path`, `hide_children_in_menu`, `explain`, `intercept`, `status`, `create_id`, `creator`, `create_time`, `modify_id`, `modifier`, `modify_time`) VALUES (4, 38, '首页-系统通知', 37, 1, 2, NULL, '/home/systemMessage', 0, NULL, NULL, 1, 1, 'admin', '2019-05-13 13:27:39', NULL, NULL, '2019-05-13 13:27:39');
INSERT INTO `m_resource`(`id`, `resource_id`, `name`, `parent_id`, `resource_type`, `resource_lev`, `icon`, `path`, `hide_children_in_menu`, `explain`, `intercept`, `status`, `create_id`, `creator`, `create_time`, `modify_id`, `modifier`, `modify_time`) VALUES (5, 39, '首页-个人中心', 37, 1, 2, NULL, '/home/personCenter', 0, NULL, NULL, 1, 1, 'admin', '2019-05-13 13:27:39', NULL, NULL, '2019-05-13 13:27:39');
INSERT INTO `m_resource`(`id`, `resource_id`, `name`, `parent_id`, `resource_type`, `resource_lev`, `icon`, `path`, `hide_children_in_menu`, `explain`, `intercept`, `status`, `create_id`, `creator`, `create_time`, `modify_id`, `modifier`, `modify_time`) VALUES (6, 40, '企业管理', 0, 1, 1, 'setting', '/companyManage', 1, NULL, NULL, 1, 1, 'admin', '2019-05-13 13:27:39', NULL, NULL, '2019-05-13 13:27:39');
INSERT INTO `m_resource`(`id`, `resource_id`, `name`, `parent_id`, `resource_type`, `resource_lev`, `icon`, `path`, `hide_children_in_menu`, `explain`, `intercept`, `status`, `create_id`, `creator`, `create_time`, `modify_id`, `modifier`, `modify_time`) VALUES (7, 41, '企业管理-查看详细', 40, 1, 2, NULL, '/companyManage/detail', 0, NULL, NULL, 1, 1, 'admin', '2019-05-13 13:27:39', NULL, NULL, '2019-05-13 13:27:39');
INSERT INTO `m_resource`(`id`, `resource_id`, `name`, `parent_id`, `resource_type`, `resource_lev`, `icon`, `path`, `hide_children_in_menu`, `explain`, `intercept`, `status`, `create_id`, `creator`, `create_time`, `modify_id`, `modifier`, `modify_time`) VALUES (8, 42, '产品开通', 0, 1, 1, 'appstore', '/openApp', 0, NULL, NULL, 1, 1, 'admin', '2019-05-13 13:27:39', NULL, NULL, '2019-05-13 13:27:39');
INSERT INTO `m_resource`(`id`, `resource_id`, `name`, `parent_id`, `resource_type`, `resource_lev`, `icon`, `path`, `hide_children_in_menu`, `explain`, `intercept`, `status`, `create_id`, `creator`, `create_time`, `modify_id`, `modifier`, `modify_time`) VALUES (9, 43, '权限-用户管理', 0, 1, 2, 'setting', '/crm', 1, NULL, NULL, 1, 1, 'admin', '2019-05-13 13:27:39', NULL, NULL, '2019-05-13 13:27:39');
INSERT INTO `m_resource`(`id`, `resource_id`, `name`, `parent_id`, `resource_type`, `resource_lev`, `icon`, `path`, `hide_children_in_menu`, `explain`, `intercept`, `status`, `create_id`, `creator`, `create_time`, `modify_id`, `modifier`, `modify_time`) VALUES (10, 44, '权限-编辑账户', 43, 1, 2, NULL, '/crm/edit', 0, NULL, NULL, 1, 1, 'admin', '2019-05-13 13:27:39', NULL, NULL, '2019-05-13 13:27:39');
INSERT INTO `m_resource`(`id`, `resource_id`, `name`, `parent_id`, `resource_type`, `resource_lev`, `icon`, `path`, `hide_children_in_menu`, `explain`, `intercept`, `status`, `create_id`, `creator`, `create_time`, `modify_id`, `modifier`, `modify_time`) VALUES (11, 45, '前台配置', 0, 1, 1, 'setting', '/setting', 1, NULL, NULL, 1, 1, 'admin', '2019-05-13 13:27:39', NULL, NULL, '2019-05-13 13:27:39');
INSERT INTO `m_resource`(`id`, `resource_id`, `name`, `parent_id`, `resource_type`, `resource_lev`, `icon`, `path`, `hide_children_in_menu`, `explain`, `intercept`, `status`, `create_id`, `creator`, `create_time`, `modify_id`, `modifier`, `modify_time`) VALUES (12, 46, '用户管理', 0, 1, 1, 'setting', '/user', 1, NULL, NULL, 1, 1, 'admin', '2019-05-13 13:27:39', NULL, NULL, '2019-05-13 13:27:39');
INSERT INTO `m_resource`(`id`, `resource_id`, `name`, `parent_id`, `resource_type`, `resource_lev`, `icon`, `path`, `hide_children_in_menu`, `explain`, `intercept`, `status`, `create_id`, `creator`, `create_time`, `modify_id`, `modifier`, `modify_time`) VALUES (13, 47, '用户管理-查看详细', 46, 1, 2, NULL, '/user/details', 0, NULL, NULL, 1, 1, 'admin', '2019-05-13 13:27:39', NULL, NULL, '2019-05-13 13:27:39');
INSERT INTO `m_resource`(`id`, `resource_id`, `name`, `parent_id`, `resource_type`, `resource_lev`, `icon`, `path`, `hide_children_in_menu`, `explain`, `intercept`, `status`, `create_id`, `creator`, `create_time`, `modify_id`, `modifier`, `modify_time`) VALUES (14, 48, '工单和反馈意见', 0, 1, 1, 'mail', '/workAndSug', 1, NULL, NULL, 1, 1, 'admin', '2019-05-13 13:27:39', NULL, NULL, '2019-05-13 13:27:39');
INSERT INTO `m_resource`(`id`, `resource_id`, `name`, `parent_id`, `resource_type`, `resource_lev`, `icon`, `path`, `hide_children_in_menu`, `explain`, `intercept`, `status`, `create_id`, `creator`, `create_time`, `modify_id`, `modifier`, `modify_time`) VALUES (15, 49, '工单和反馈意见-工单详细', 48, 1, 2, NULL, '/workAndSug/workDetails', 0, NULL, NULL, 1, 1, 'admin', '2019-05-13 13:27:39', NULL, NULL, '2019-05-13 13:27:39');
INSERT INTO `m_resource`(`id`, `resource_id`, `name`, `parent_id`, `resource_type`, `resource_lev`, `icon`, `path`, `hide_children_in_menu`, `explain`, `intercept`, `status`, `create_id`, `creator`, `create_time`, `modify_id`, `modifier`, `modify_time`) VALUES (16, 50, '工单和反馈意见-意见详细', 48, 1, 2, NULL, '/workAndSug/sugDetails', 0, NULL, NULL, 1, 1, 'admin', '2019-05-13 13:27:39', NULL, NULL, '2019-05-13 13:27:39');
INSERT INTO `m_resource`(`id`, `resource_id`, `name`, `parent_id`, `resource_type`, `resource_lev`, `icon`, `path`, `hide_children_in_menu`, `explain`, `intercept`, `status`, `create_id`, `creator`, `create_time`, `modify_id`, `modifier`, `modify_time`) VALUES (17, 51, '资讯管理', 0, 1, 1, 'mail', '/consultation', 1, NULL, NULL, 1, 1, 'admin', '2019-05-13 13:27:39', NULL, NULL, '2019-05-13 13:27:39');
INSERT INTO `m_resource`(`id`, `resource_id`, `name`, `parent_id`, `resource_type`, `resource_lev`, `icon`, `path`, `hide_children_in_menu`, `explain`, `intercept`, `status`, `create_id`, `creator`, `create_time`, `modify_id`, `modifier`, `modify_time`) VALUES (18, 52, '资讯管理-编辑资讯', 51, 1, 2, NULL, '/consultation/edit', 0, NULL, NULL, 1, 1, 'admin', '2019-05-13 13:27:39', NULL, NULL, '2019-05-13 13:27:39');
INSERT INTO `m_resource`(`id`, `resource_id`, `name`, `parent_id`, `resource_type`, `resource_lev`, `icon`, `path`, `hide_children_in_menu`, `explain`, `intercept`, `status`, `create_id`, `creator`, `create_time`, `modify_id`, `modifier`, `modify_time`) VALUES (19, 53, '权限管理', 0, 1, 1, 'setting', '/permission', 1, NULL, NULL, 1, 1, 'admin', '2019-05-13 13:27:39', NULL, NULL, '2019-05-13 13:27:39');
INSERT INTO `m_resource`(`id`, `resource_id`, `name`, `parent_id`, `resource_type`, `resource_lev`, `icon`, `path`, `hide_children_in_menu`, `explain`, `intercept`, `status`, `create_id`, `creator`, `create_time`, `modify_id`, `modifier`, `modify_time`) VALUES (20, 54, '编辑权限组', 53, 1, 1, NULL, '/permission/edit', 0, NULL, NULL, 1, 1, 'admin', '2019-05-13 13:27:39', NULL, NULL, '2019-05-13 13:27:39');
INSERT INTO `m_resource`(`id`, `resource_id`, `name`, `parent_id`, `resource_type`, `resource_lev`, `icon`, `path`, `hide_children_in_menu`, `explain`, `intercept`, `status`, `create_id`, `creator`, `create_time`, `modify_id`, `modifier`, `modify_time`) VALUES (22, 55, '系统帮助', 0, 1, 1, 'setting', '/sysHelp', 1, NULL, NULL, 1, 1, 'admin', '2019-05-13 13:27:39', NULL, NULL, '2019-05-13 13:27:39');
INSERT INTO `m_resource`(`id`, `resource_id`, `name`, `parent_id`, `resource_type`, `resource_lev`, `icon`, `path`, `hide_children_in_menu`, `explain`, `intercept`, `status`, `create_id`, `creator`, `create_time`, `modify_id`, `modifier`, `modify_time`) VALUES (23, 56, '首页大类编辑', 55, 1, 1, NULL, '/sysHelp/homeBigClassEdit', 0, NULL, NULL, 1, 1, 'admin', '2019-05-13 13:27:39', NULL, NULL, '2019-05-13 13:27:39');
INSERT INTO `m_resource`(`id`, `resource_id`, `name`, `parent_id`, `resource_type`, `resource_lev`, `icon`, `path`, `hide_children_in_menu`, `explain`, `intercept`, `status`, `create_id`, `creator`, `create_time`, `modify_id`, `modifier`, `modify_time`) VALUES (24, 57, '常见问题大类编辑', 55, 1, 1, NULL, '/sysHelp/normalBigClassEdit', 0, NULL, NULL, 1, 1, 'admin', '2019-05-13 13:27:39', NULL, NULL, '2019-05-13 13:27:39');
INSERT INTO `m_resource`(`id`, `resource_id`, `name`, `parent_id`, `resource_type`, `resource_lev`, `icon`, `path`, `hide_children_in_menu`, `explain`, `intercept`, `status`, `create_id`, `creator`, `create_time`, `modify_id`, `modifier`, `modify_time`) VALUES (25, 58, '常见问题小类编辑', 57, 1, 1, NULL, '/sysHelp/normalBigClassEdit/normalSmallClassEdit', 0, NULL, NULL, 1, 1, 'admin', '2019-05-13 13:27:39', NULL, NULL, '2019-05-13 13:27:39');
INSERT INTO `m_resource`(`id`, `resource_id`, `name`, `parent_id`, `resource_type`, `resource_lev`, `icon`, `path`, `hide_children_in_menu`, `explain`, `intercept`, `status`, `create_id`, `creator`, `create_time`, `modify_id`, `modifier`, `modify_time`) VALUES (26, 59, '常见问题内容编辑', 58, 1, 1, NULL, '/sysHelp/normalBigClassEdit/normalSmallClassEdit/normalTextEdit', 0, NULL, NULL, 1, 1, 'admin', '2019-05-13 13:27:39', NULL, NULL, '2019-05-13 13:27:39');
INSERT INTO `m_resource`(`id`, `resource_id`, `name`, `parent_id`, `resource_type`, `resource_lev`, `icon`, `path`, `hide_children_in_menu`, `explain`, `intercept`, `status`, `create_id`, `creator`, `create_time`, `modify_id`, `modifier`, `modify_time`) VALUES (27, 60, '企业管理-编辑按钮', 41, 3, 3, NULL, NULL, NULL, NULL, NULL, 1, 1, 'admin', '2019-05-13 13:27:39', NULL, NULL, '2019-05-13 13:27:39');
INSERT INTO `m_resource`(`id`, `resource_id`, `name`, `parent_id`, `resource_type`, `resource_lev`, `icon`, `path`, `hide_children_in_menu`, `explain`, `intercept`, `status`, `create_id`, `creator`, `create_time`, `modify_id`, `modifier`, `modify_time`) VALUES (28, 61, '产品开通-编辑按钮', 42, 3, 3, NULL, NULL, NULL, NULL, NULL, 1, 1, 'admin', '2019-05-13 13:27:39', NULL, NULL, '2019-05-13 13:27:39');
INSERT INTO `m_resource`(`id`, `resource_id`, `name`, `parent_id`, `resource_type`, `resource_lev`, `icon`, `path`, `hide_children_in_menu`, `explain`, `intercept`, `status`, `create_id`, `creator`, `create_time`, `modify_id`, `modifier`, `modify_time`) VALUES (29, 62, '用户管理-编辑按钮', 47, 3, 3, NULL, NULL, NULL, NULL, NULL, 1, 1, 'admin', '2019-05-13 13:27:39', NULL, NULL, '2019-05-13 13:27:39');
INSERT INTO `m_resource`(`id`, `resource_id`, `name`, `parent_id`, `resource_type`, `resource_lev`, `icon`, `path`, `hide_children_in_menu`, `explain`, `intercept`, `status`, `create_id`, `creator`, `create_time`, `modify_id`, `modifier`, `modify_time`) VALUES (30, 63, '前台配置-编辑按钮', 45, 3, 3, NULL, NULL, NULL, NULL, NULL, 1, 1, 'admin', '2019-05-13 13:27:39', NULL, NULL, '2019-05-13 13:27:39');
INSERT INTO `m_resource`(`id`, `resource_id`, `name`, `parent_id`, `resource_type`, `resource_lev`, `icon`, `path`, `hide_children_in_menu`, `explain`, `intercept`, `status`, `create_id`, `creator`, `create_time`, `modify_id`, `modifier`, `modify_time`) VALUES (31, 64, '工单详细-编辑按钮', 49, 3, 3, NULL, NULL, NULL, NULL, NULL, 1, 1, 'admin', '2019-05-13 13:27:39', NULL, NULL, '2019-05-13 13:27:39');
INSERT INTO `m_resource`(`id`, `resource_id`, `name`, `parent_id`, `resource_type`, `resource_lev`, `icon`, `path`, `hide_children_in_menu`, `explain`, `intercept`, `status`, `create_id`, `creator`, `create_time`, `modify_id`, `modifier`, `modify_time`) VALUES (32, 65, '资讯管理-编辑按钮', 51, 3, 3, NULL, NULL, NULL, NULL, NULL, 1, 1, 'admin', '2019-05-13 13:27:39', NULL, NULL, '2019-05-13 13:27:39');
INSERT INTO `m_resource`(`id`, `resource_id`, `name`, `parent_id`, `resource_type`, `resource_lev`, `icon`, `path`, `hide_children_in_menu`, `explain`, `intercept`, `status`, `create_id`, `creator`, `create_time`, `modify_id`, `modifier`, `modify_time`) VALUES (33, 66, '系统帮助-编辑按钮', 55, 3, 3, NULL, NULL, NULL, NULL, NULL, 1, 1, 'admin', '2019-05-13 13:27:39', NULL, NULL, '2019-05-13 13:27:39');
INSERT INTO `m_resource`(`id`, `resource_id`, `name`, `parent_id`, `resource_type`, `resource_lev`, `icon`, `path`, `hide_children_in_menu`, `explain`, `intercept`, `status`, `create_id`, `creator`, `create_time`, `modify_id`, `modifier`, `modify_time`) VALUES (34, 67, '权限配置-编辑按钮', 53, 3, 3, NULL, NULL, NULL, NULL, NULL, 1, 1, 'admin', '2019-05-13 13:27:39', NULL, NULL, '2019-05-13 13:27:39');
INSERT INTO `m_resource`(`id`, `resource_id`, `name`, `parent_id`, `resource_type`, `resource_lev`, `icon`, `path`, `hide_children_in_menu`, `explain`, `intercept`, `status`, `create_id`, `creator`, `create_time`, `modify_id`, `modifier`, `modify_time`) VALUES (35, 68, '权限用户管理-编辑按钮', 43, 3, 3, NULL, NULL, NULL, NULL, NULL, 1, 1, 'admin', '2019-05-13 13:27:39', NULL, NULL, '2019-05-13 13:27:39');

-- Table structure for m_platform_managers
-- ----------------------------
DROP TABLE IF EXISTS `m_platform_managers`;
CREATE TABLE `m_platform_managers`  (
  `manager_id` int(10) UNSIGNED NOT NULL AUTO_INCREMENT COMMENT 'id',
  `beesrv_id` varchar(50) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '管理员编号',
  `username` varchar(32) CHARACTER SET utf8 COLLATE utf8_general_ci NOT NULL COMMENT '账户',
  `phone` varchar(11) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '手机号',
  `sys_token` varchar(200) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '系统登录凭证',
  `expires_in` datetime(0) DEFAULT NULL COMMENT 'token失效时间',
  `head` varchar(200) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '头像地址',
  `email` varchar(120) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '邮箱',
  `password` varchar(100) CHARACTER SET utf8 COLLATE utf8_general_ci NOT NULL COMMENT '密码',
  `nickname` varchar(40) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '姓名',
  `create_at` datetime(0) DEFAULT NULL COMMENT '创建时间',
  `update_at` datetime(0) DEFAULT NULL COMMENT '修改时间',
  `notes` varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '账户说明',
  `status` tinyint(1) NOT NULL COMMENT '账户状态',
  `creator_id` int(10) NOT NULL COMMENT '修改人id',
  `update_id` int(10) DEFAULT NULL COMMENT '修改人id',
  PRIMARY KEY (`manager_id`) USING BTREE,
  UNIQUE INDEX `username`(`username`) USING BTREE
) ENGINE = InnoDB AUTO_INCREMENT = 20 CHARACTER SET = utf8 COLLATE = utf8_general_ci COMMENT = '后台管理用户列表' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of m_platform_managers
-- ----------------------------
INSERT INTO `m_platform_managers` VALUES (1, '19051200000401', '18581990837', '18581990837', 'platform-f2e6e1d8-03be-4689-ad55-2700047810cc-rhKKIP', '2019-05-23 09:35:10', 'string', '358736504@qq.com', '$2a$10$VD9odLfugCEETUvrZxH/punWeqjdNkCzLujzgx9xkcF7Q.246W9ea', '李俊杨', '2016-12-01 12:30:45', '2019-05-22 09:35:10', '测试水水水水水水水水', 1, 0, 1);

-- ----------------------------
-- Table structure for m_notice_template
-- ----------------------------
DROP TABLE IF EXISTS `m_notice_template`;
CREATE TABLE `m_notice_template`  (
  `template_id` int(10) NOT NULL AUTO_INCREMENT COMMENT '模板id',
  `template_name` varchar(60) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci DEFAULT NULL COMMENT '模板名称',
  `template_type` tinyint(1) DEFAULT NULL COMMENT '模板类型',
  `notice_title` varchar(60) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci DEFAULT NULL COMMENT '通知标题',
  `notify_object` varchar(60) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci DEFAULT NULL COMMENT '通知对象',
  `notify_object_type` tinyint(1) DEFAULT NULL COMMENT '通知对象类型',
  `template_content` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci DEFAULT NULL COMMENT '模板内容',
  `notify_mode` tinyint(1) DEFAULT NULL COMMENT '通知方式',
  `notes` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci DEFAULT NULL COMMENT '备注',
  `update_time` datetime(0) DEFAULT NULL COMMENT '最后编辑时间',
  `status` tinyint(1) DEFAULT NULL COMMENT '是否有效',
  PRIMARY KEY (`template_id`) USING BTREE,
  INDEX ```template_type```(`template_type`) USING BTREE COMMENT '模板类型'
) ENGINE = InnoDB AUTO_INCREMENT = 28 CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '后台管理系统通知模板' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of m_notice_template
-- ----------------------------
INSERT INTO `m_notice_template` VALUES (1, '发送验证码', 1, '等待手机验证', '操作人', 1, '系统已向您尾号为【{0}】的手机发送验证码，请注意查收！验证码有效期为15分钟，请在有效期内进行验证操作，过期需重新发送验证码！', 1, NULL, '2019-04-28 00:00:00', 1);
INSERT INTO `m_notice_template` VALUES (3, '自主修改手机完成', 3, '修改手机号码通知', '操作人', 1, '您的手机号已修改为尾号为【{0}】的号码，修改时间为【{1}】。', 1, NULL, '2019-04-28 00:00:00', 1);
INSERT INTO `m_notice_template` VALUES (4, '自主修改邮箱完成', 4, '修改邮箱通知', '操作人', 1, '您的邮箱已修改为【{0}】，修改时间为【{1}】。', 2, '邮箱显示规则：取前面2位字母，在后面加上三个 * 号，@及后面部分全部显示。举例：asdfdasdf@qq.com显示为：as***@qq.com', '2019-04-28 00:00:00', 1);
INSERT INTO `m_notice_template` VALUES (5, '用户完成修改密码', 5, '修改密码通知', '操作人', 1, '您的密码已修改成功，修改时间为【{0}】。', 1, NULL, '2019-04-28 00:00:00', 1);
INSERT INTO `m_notice_template` VALUES (6, '自主修改其他个人资料完成', 6, '修改基本资料通知', '操作人', 1, '您的个人资料已修改成功，修改时间为【{0}】。', 1, NULL, '2019-04-28 00:00:00', 1);
INSERT INTO `m_notice_template` VALUES (7, '企业申请', 7, '企业入驻申请', '具有企业审核权限的角色', 5, '有新企业【{0}】申请入驻，请尽快处理。', 3, NULL, '2019-04-28 00:00:00', 1);
INSERT INTO `m_notice_template` VALUES (8, '企业申请审批', 8, '企业入驻审批', '具有企业审核权限的角色', 5, '您已{0}【{1}】的入驻申请。', 1, NULL, '2019-04-28 00:00:00', 1);
INSERT INTO `m_notice_template` VALUES (9, '企业管理员完成变更', 9, '企业管理员变更', '操作人', 1, '您已将【{0}】的管理员更换为【{1}】。', 1, NULL, '2019-04-28 00:00:00', 1);
INSERT INTO `m_notice_template` VALUES (10, '企业管理员完成变更', 10, '企业管理员变更', '旧企业管理员', 6, '您的【{0}】企业管理员身份已被取消，如有疑问请联系后台管理员。', 4, NULL, '2019-04-28 00:00:00', 1);
INSERT INTO `m_notice_template` VALUES (11, '企业管理员完成变更', 11, '企业管理员变更', '新企业管理员', 7, '您已被设为【{0}】的企业管理员，如有疑问请联系后台管理员。', 4, NULL, '2019-04-28 00:00:00', 1);
INSERT INTO `m_notice_template` VALUES (12, '产品开通申请', 12, '产品开通申请', '具有产品开通审核权限的角色', 4, '有企业【{0}】申请开通【{1} |{2}、{3}】，请尽快处理。', 1, NULL, '2019-04-28 00:00:00', 1);
INSERT INTO `m_notice_template` VALUES (13, '产品开通审批', 13, '产品开通申请', '具有产品开通审核权限的角色', 4, '您已{0}【{1}】【{2} | {3}】的开通申请。', 1, NULL, '2019-04-28 00:00:00', 1);
INSERT INTO `m_notice_template` VALUES (14, '管理员启用/禁用用户账户', 14, '用户状态变更', '操作人', 1, '您已将{0}的{1}的账号{2}。', 1, NULL, '2019-04-28 00:00:00', 1);
INSERT INTO `m_notice_template` VALUES (15, '编辑用户资料完成', 15, '用户资料变更通知', '操作人', 1, '您已将用户【{0}】的资料进行了变更。', 1, NULL, '2019-04-28 00:00:00', 1);
INSERT INTO `m_notice_template` VALUES (16, '前台配置完成', 16, '前台配置变更', '操作人', 1, '您已修改了前台配置【{0}-{1}】。', 1, NULL, '2019-04-28 00:00:00', 1);
INSERT INTO `m_notice_template` VALUES (17, '工单提交', 17, '新工单提交', '具有工单处理权限的角色', 3, '用户【{0}】提交了新的工单【{1}】，请尽快前往处理。', 3, NULL, '2019-04-28 00:00:00', 1);
INSERT INTO `m_notice_template` VALUES (18, '工单回复', 18, '工单状态变化', '具有工单处理权限的角色', 3, '【{0}】的工单已有回复，请尽快前往处理。', 3, NULL, '2019-04-28 00:00:00', 1);
INSERT INTO `m_notice_template` VALUES (19, '工单关闭', 19, '工单关闭', '具有工单处理权限的角色', 3, '用户【{0}】提交的【{1}】的工单问题已解决，状态变为已关闭。', 1, NULL, '2019-04-28 00:00:00', 1);
INSERT INTO `m_notice_template` VALUES (20, '管理员重置后台账号密码', 20, '管理员重置密码', '操作人', 1, '您已对后台账户【{0}】的账号密码进行了重置。', 1, NULL, '2019-04-28 00:00:00', 1);
INSERT INTO `m_notice_template` VALUES (21, '管理员重置后台账号密码', 21, '管理员重置密码', '被操作人', 2, '您的后台账号密码已管理员重置，新密码已通过短信发送到您尾号为【{0}】的手机上。', 3, NULL, '2019-04-28 00:00:00', 1);
INSERT INTO `m_notice_template` VALUES (23, '管理员设置后台权限', 23, '权限组修改', '操作人', 1, '您已修改了权限组【{0}】的权限。', 1, NULL, '2019-04-28 00:00:00', 1);
INSERT INTO `m_notice_template` VALUES (24, '管理员设置后台权限', 24, '用户权限修改', '操作人', 1, '您已修改了【{0}】的后台权限。', 1, NULL, '2019-04-28 00:00:00', 1);
INSERT INTO `m_notice_template` VALUES (26, '修改企业信息', 26, '企业信息变更通知', '具有企业审核权限的角色', 5, '有企业【{0}】申请修改信息，请尽快处理。', 3, NULL, '2019-04-28 00:00:00', 1);
INSERT INTO `m_notice_template` VALUES (27, '修改企业信息', 27, '企业信息变更通知', '操作人', 1, '您已{0}【{1}】的修改信息申请。', 1, NULL, '2019-04-28 00:00:00', 1);
INSERT INTO `st_platform`.`m_notice_template`(`template_id`, `template_name`, `template_type`, `notice_title`, `notify_object`, `notify_object_type`, `template_content`, `notify_mode`, `notes`, `update_time`, `status`) VALUES (28, '管理员邮件重置后台账号密码', 28, '管理员重置密码', '被操作人', 2, '您的后台账号密码已被管理员重置，新密码已发送到您{0}的邮箱中，请注意查收！', 3, NULL, '2019-05-21 16:37:55', 1);

-- 意见反馈表
CREATE TABLE `feedback` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT COMMENT 'id',
  `advice_user` varchar(20) DEFAULT NULL COMMENT '建议人',
  `advice_type` tinyint(1) DEFAULT NULL COMMENT '建议类型',
  `advice_title` varchar(255) DEFAULT '' COMMENT '建议标题',
  `is_check` tinyint(1) DEFAULT '0' COMMENT '是否查看（0未查看 1查看）',
  `content` varchar(1024) DEFAULT NULL COMMENT '意见内容',
  `files` varchar(512) DEFAULT NULL COMMENT '附件',
  `status` tinyint(1) DEFAULT NULL COMMENT '是否有效（0无效 1有效）',
  `create_time` datetime DEFAULT NULL COMMENT '提交时间',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=13 DEFAULT CHARSET=utf8mb4 COMMENT='意见反馈';

-- 工单信息表
CREATE TABLE `st_platform`.`work_orders`  (
  `id` bigint(11) NOT NULL AUTO_INCREMENT COMMENT 'ID',
  `work_order_number` varchar(25) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NULL DEFAULT NULL COMMENT '工单编号',
  `work_order_title` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NULL DEFAULT NULL COMMENT '工单标题',
  `priority` int(2) NULL DEFAULT NULL COMMENT '优先级2重要1一般',
  `order_status` int(2) NULL DEFAULT NULL COMMENT '工单状态 1待平台处理、2平台处理中、3待用户确认、4已关闭',
  `belong_app` int(2) NULL DEFAULT NULL COMMENT '问题所属产品',
  `belong_app_name` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NULL DEFAULT NULL COMMENT '问题所属产品名称',
  `belong_company` int(10) NULL DEFAULT NULL COMMENT '提交人所属公司',
  `belong_company_name` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NULL DEFAULT NULL COMMENT '提交人所属公司名称',
  `role_name` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NULL DEFAULT NULL COMMENT '提交人在所属公司角色名称',
  `problem_description` varchar(1200) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NULL DEFAULT NULL COMMENT '问题描述',
  `phone` varchar(20) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NULL DEFAULT NULL COMMENT '联系手机号码',
  `enclosure` varchar(1024) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NULL DEFAULT NULL COMMENT '附件',
  `submit_time` datetime(0) NULL DEFAULT NULL COMMENT '提交时间',
  `recent_accept_time` datetime(0) NULL DEFAULT NULL COMMENT '最近受理时间',
  `accept_id` bigint(20) NULL DEFAULT NULL COMMENT '受理人id',
  `acceptor` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NULL DEFAULT NULL COMMENT '受理人',
  `status` int(2) NULL DEFAULT NULL COMMENT '数据状态0删除1正常',
  `create_id` bigint(20) NULL DEFAULT NULL COMMENT '创建人id',
  `create_head` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NULL DEFAULT NULL COMMENT '创建人头像',
  `creator` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NULL DEFAULT NULL COMMENT '创建人',
  `create_time` datetime(0) NULL DEFAULT NULL COMMENT '创建时间',
  `modify_id` bigint(20) NULL DEFAULT NULL COMMENT '修改人id',
  `modifier` varchar(20) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NULL DEFAULT NULL COMMENT '修改人名称',
  `modify_time` datetime(0) NULL DEFAULT NULL COMMENT '修改时间',
  `remark` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NULL DEFAULT '' COMMENT '其他信息',
  PRIMARY KEY (`id`) USING BTREE,
  UNIQUE INDEX `work_orders_id_index`(`id`) USING BTREE COMMENT '工单信息主键id索引',
  INDEX `work_orders_number_index`(`work_order_number`) USING BTREE COMMENT '工单编号索引'
) ENGINE = InnoDB AUTO_INCREMENT = 23 CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '工单信息表' ROW_FORMAT = Dynamic;

-- 工单沟通详情表
CREATE TABLE `st_platform`.`work_orders_detail`  (
  `id` bigint(11) NOT NULL AUTO_INCREMENT COMMENT 'ID',
  `work_order_number` varchar(20) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NULL DEFAULT NULL COMMENT '工单编号',
  `reply_description` varchar(1000) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NULL DEFAULT NULL COMMENT '回复详情',
  `enclosure` varchar(1024) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NULL DEFAULT NULL COMMENT '附件',
  `reply_id` bigint(20) NULL DEFAULT NULL COMMENT '回复人id',
  `reply_head` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NULL DEFAULT NULL COMMENT '回复人头像',
  `reply_name` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NULL DEFAULT NULL COMMENT '回复人',
  `reply_time` datetime(0) NULL DEFAULT NULL COMMENT '回复时间',
  PRIMARY KEY (`id`) USING BTREE,
  UNIQUE INDEX `work_orders_detail_id_index`(`id`) USING BTREE COMMENT '工单沟通详情主键id索引'
) ENGINE = InnoDB AUTO_INCREMENT = 16 CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '工单沟通详情表' ROW_FORMAT = Dynamic;

-- 序列表
CREATE TABLE `st_platform`.`sequence`  (
  `id` bigint(20) NOT NULL,
  `sequence_key` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL COMMENT '序列名称',
  `sequence_value` int(20) NOT NULL COMMENT '序列值',
  `status` tinyint(2) NOT NULL COMMENT '状态 0 无效 1 有效',
  `create_id` bigint(20) NOT NULL COMMENT '创建人id',
  `creator` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL COMMENT '创建人',
  `create_time` datetime(0) NOT NULL COMMENT '创建时间',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci ROW_FORMAT = Dynamic;

INSERT INTO `st_platform`.`sequence`(`id`, `sequence_key`, `sequence_value`, `status`, `create_id`, `creator`, `create_time`) VALUES (1, 'workOrdersSeq', 11000, 1, 1, '超级管理员', '2019-04-28 10:23:30');

-- 公告类型表
DROP TABLE IF EXISTS `articles_type`;
CREATE TABLE `articles_type`  (
  `id` int(10) UNSIGNED NOT NULL AUTO_INCREMENT,
  `name` varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci NULL DEFAULT NULL COMMENT '公告类型名',
  `org_id` int(10) NULL DEFAULT NULL COMMENT '企业id',
  `status` tinyint(2) NULL DEFAULT NULL COMMENT '0无效，1有效',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB AUTO_INCREMENT = 5 CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci ROW_FORMAT = Dynamic;
INSERT INTO `articles_type` VALUES (1, '集团发文', 1,1);
INSERT INTO `articles_type` VALUES (2, '规章制度', 1,1);
INSERT INTO `articles_type` VALUES (3, '内部资讯', 1,1);
INSERT INTO `articles_type` VALUES (4, '督查通报', 1,1);

-- 部门加描述
alter table `departments` ADD description VARCHAR(255) Default null COMMENT '部门描述';

-- 企业开通的产品、角色表
DROP TABLE IF EXISTS `enterprises_apps`;
CREATE TABLE `enterprises_apps` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `org_id` int(10) unsigned DEFAULT NULL COMMENT '公司主键',
  `org_name` varchar(255) DEFAULT NULL COMMENT '企业名称',
  `app_id` int(10) unsigned DEFAULT NULL COMMENT '产品主键',
  `app_roles_id` int(10) DEFAULT NULL COMMENT '产品角色id',
  `aduit_state` tinyint(8) DEFAULT NULL COMMENT '审核状态(0: 未通过，1: 已通过, 2: 待审核)',
  `reject_reason` varchar(255) DEFAULT NULL COMMENT '拒绝原因',
  `url` varchar(255) DEFAULT NULL,
  `status` tinyint(2) DEFAULT NULL COMMENT '数据状态，0-无效，1-有效',
  `create_id` int(10) DEFAULT NULL COMMENT '创建人id',
  `create_time` datetime DEFAULT NULL COMMENT '创建时间(提交时间)',
  `creator` varchar(255) DEFAULT NULL COMMENT '创建人',
  `modify_id` int(10) DEFAULT NULL COMMENT '修改人id',
  `modify_time` datetime DEFAULT NULL COMMENT '修改时间',
  `modifier` varchar(255) DEFAULT NULL COMMENT '修改人',
  PRIMARY KEY (`id`),
  KEY `app_id_index` (`app_id`) USING BTREE,
  KEY `org_id_index` (`org_id`) USING BTREE
) ENGINE=InnoDB AUTO_INCREMENT=130 DEFAULT CHARSET=utf8 COMMENT='企业开通的产品、角色表';

-- 工作台任务表
CREATE TABLE `workbench_task` (
  `id` bigint(11) NOT NULL AUTO_INCREMENT COMMENT 'ID',
  `task_id` varchar(25) DEFAULT NULL COMMENT '任务编号',
  `task_type` int(2) DEFAULT NULL COMMENT '任务类型 1线上蜂贸、2蜂创物联、3领蜂供应链、4集蜂联运、5金蜜ERP',
  `backlog_content` varchar(255) DEFAULT NULL COMMENT '待办内容',
  `task_statu` int(2) DEFAULT NULL COMMENT '任务状态 0待处理、1已处理',
  `deal_url` varchar(255) DEFAULT NULL COMMENT '子系统处理地址',
  `status` int(2) DEFAULT NULL COMMENT '数据状态0删除1正常',
  `create_id` bigint(20) DEFAULT NULL COMMENT '创建人id',
  `creator` varchar(128) DEFAULT NULL COMMENT '创建人',
  `create_time` datetime DEFAULT NULL COMMENT '创建时间',
  `modify_id` bigint(20) DEFAULT NULL COMMENT '修改人id',
  `modifier` varchar(20) DEFAULT NULL COMMENT '修改人名称',
  `modify_time` datetime DEFAULT NULL COMMENT '修改时间',
  `remark` varchar(128) DEFAULT '' COMMENT '其他信息',
  PRIMARY KEY (`id`),
  UNIQUE KEY `id_index` (`id`) USING BTREE COMMENT '任务主键id索引'
) ENGINE=InnoDB AUTO_INCREMENT=31 DEFAULT CHARSET=utf8mb4 COMMENT='工作台任务表';

-- 系统帮助 文章表
DROP TABLE IF EXISTS `m_content_classification`;
CREATE TABLE `m_content_classification` (
  `id` int(11) NOT NULL AUTO_INCREMENT COMMENT '文章内容主键id',
  `name` varchar(20) NOT NULL COMMENT '文章标题',
  `weights` int(11) NOT NULL COMMENT '显示排序数值',
  `p_id` int(11) NOT NULL COMMENT '关联小类id',
  `mobile_content` text NOT NULL COMMENT '移动端展示内容',
  `pc_content` text NOT NULL COMMENT 'pc端展示内容',
  `status` tinyint(4) NOT NULL COMMENT '状态标识，0代表使用中，1代表删除',
  `create_uid` bigint(20) DEFAULT NULL COMMENT '创建人id',
  `create_time` datetime DEFAULT NULL COMMENT '创建时间',
  `update_uid` bigint(20) DEFAULT NULL COMMENT '更新人id',
  `update_time` datetime DEFAULT NULL COMMENT '更新时间',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- 帮助首页关联表
CREATE TABLE `m_helper_relation` (
  `id` int(11) NOT NULL AUTO_INCREMENT COMMENT '主键id',
  `p_id` int(11) NOT NULL COMMENT '帮助首页大类id',
  `s_id` int(11) NOT NULL,
  `c_id` int(11) NOT NULL,
  `weights` int(11) NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- 帮助系统大类表
CREATE TABLE `m_primary_classification` (
  `id` int(11) NOT NULL AUTO_INCREMENT COMMENT '大类主键id',
  `name` varchar(20) NOT NULL COMMENT '大类名称',
  `weights` int(11) NOT NULL COMMENT '显示排序，数值越小越靠前',
  `classify_type` int(11) NOT NULL COMMENT '大类分类 (0 代表帮助首页大类，1 代表常见问题大类，2代表用户指南大类)',
  `status` tinyint(4) NOT NULL COMMENT '状态，0使用中，1删除',
  `create_time` datetime DEFAULT NULL COMMENT '创建时间',
  `update_time` datetime DEFAULT NULL COMMENT '更新时间',
  `create_uid` bigint(20) DEFAULT NULL COMMENT '创建人id',
  `update_uid` bigint(20) DEFAULT NULL COMMENT '更新人id',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;


-- ----------------------------
-- Table structure for m_operator_log
-- ----------------------------
DROP TABLE IF EXISTS `m_operator_log`;
CREATE TABLE `m_operator_log`  (
  `id` int(11) NOT NULL AUTO_INCREMENT COMMENT '主键',
  `operator_id` int(11) DEFAULT NULL COMMENT '操作人id',
  `operator_role_id` int(11) DEFAULT NULL COMMENT '操作人角色',
  `operator_role_name` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci DEFAULT NULL COMMENT '操作人角色名称',
  `operator_content` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci DEFAULT NULL COMMENT '操作内容',
  `operator_time` datetime(0) DEFAULT NULL COMMENT '操作时间',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB AUTO_INCREMENT = 40 CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci ROW_FORMAT = Dynamic;

-- 系统帮助小类表
CREATE TABLE `m_secondary_classification` (
  `id` int(11) NOT NULL AUTO_INCREMENT COMMENT '小类主键标识id',
  `p_id` int(11) NOT NULL COMMENT '关联大类id',
  `name` varchar(20) NOT NULL COMMENT '小类名称',
  `weights` int(11) NOT NULL COMMENT '显示排序数值,值越小越靠前',
  `status` tinyint(4) NOT NULL COMMENT '状态标识，0代表使用中，1代表删除',
  `create_time` datetime DEFAULT NULL COMMENT '创建时间',
  `update_time` datetime DEFAULT NULL COMMENT '更新时间',
  `create_uid` bigint(20) DEFAULT NULL COMMENT '创建人id',
  `update_uid` bigint(20) DEFAULT NULL COMMENT '更新人id',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- 资讯类型表
DROP TABLE IF EXISTS `news_type`;
CREATE TABLE `news_type` (
  `id` bigint(20) NOT NULL,
  `type_name` varchar(20) DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
INSERT INTO `news_type` VALUES ('0', '最新活动');
INSERT INTO `news_type` VALUES ('1', '金蜜快讯');
INSERT INTO `news_type` VALUES ('2', '分析评论');
INSERT INTO `news_type` VALUES ('3', '人工智能');
INSERT INTO `news_type` VALUES ('4', '行业资讯');

INSERT INTO `st_platform`.`t_config` (`config_key`, `config_value`, `config_desc`, `status`, `create_time`, `update_time`) VALUES ( 'appRoles_audit_roleId', '9', '具有产品开通审核权限的角色,m_role_role中childId为9', '1', '2019-05-14 15:21:01', '2019-05-14 15:21:24');
INSERT INTO `st_platform`.`t_config` (`config_key`, `config_value`, `config_desc`, `status`, `create_time`, `update_time`) VALUES ('enterprisesCheck_audit_roleId', '6', '具有企业管理的编辑权限的角色', '1', '2019-05-15 11:11:59', '2019-05-15 15:18:43');




ALTER TABLE `index_init`
MODIFY COLUMN `transport_volume`  decimal(10,4) NOT NULL COMMENT '集峰联运-运量' AFTER `finished_order_cnt`;

-- 角色名称用汉字显示
UPDATE `st_platform`.`u_roles` SET `name` = '企业管理员' WHERE `id` = 1;
UPDATE `st_platform`.`u_roles` SET `name` = '普通用户' WHERE `id` = 3;
-- 删除无用的角色
delete from u_roles where id = 4;


-- ----------------------------
-- Table structure for m_system_notice
-- ----------------------------
DROP TABLE IF EXISTS `m_system_notice`;
CREATE TABLE `m_system_notice`  (
  `notice_id` bigint(20) NOT NULL AUTO_INCREMENT COMMENT '通知id',
  `notifier_id` int(10) NOT NULL COMMENT '通知人id',
  `notice_title` varchar(60) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL COMMENT '通知标题',
  `notice_content` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci DEFAULT NULL COMMENT '通知内容',
  `read` tinyint(1) NOT NULL COMMENT '是否阅读，0未读，1已读',
  `status` tinyint(1) NOT NULL COMMENT '是否有效，0无效，1有效',
  `create_time` datetime(0) NOT NULL COMMENT '创建时间',
  `update_time` datetime(0) NOT NULL COMMENT '修改时间',
  PRIMARY KEY (`notice_id`) USING BTREE,
  INDEX `notifiter_id`(`notifier_id`) USING BTREE COMMENT '通知人id索引'
) ENGINE = InnoDB AUTO_INCREMENT = 181 CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '管理后台系统通知' ROW_FORMAT = Dynamic;


-- ----------------------------
-- Table structure for m_operator_log
-- ----------------------------
DROP TABLE IF EXISTS `m_operator_log`;
CREATE TABLE `m_operator_log`  (
  `id` int(11) NOT NULL AUTO_INCREMENT COMMENT '主键',
  `operator_id` int(11) DEFAULT NULL COMMENT '操作人id',
  `operator_role_id` int(11) DEFAULT NULL COMMENT '操作人角色',
  `operator_role_name` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci DEFAULT NULL COMMENT '操作人角色名称',
  `operator_content` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci DEFAULT NULL COMMENT '操作内容',
  `operator_time` datetime(0) DEFAULT NULL COMMENT '操作时间',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB AUTO_INCREMENT = 40 CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci ROW_FORMAT = Dynamic;



INSERT INTO `st_platform`.`t_config` (`config_key`, `config_value`, `config_desc`, `status`, `create_time`, `update_time`) VALUES ( 'appRoles_audit_roleId', '9', '具有产品开通审核权限的角色,m_role_role中childId为9', '1', '2019-05-14 15:21:01', '2019-05-14 15:21:24');
INSERT INTO `st_platform`.`t_config` (`config_key`, `config_value`, `config_desc`, `status`, `create_time`, `update_time`) VALUES ('enterprisesCheck_audit_roleId', '6', '具有企业管理的编辑权限的角色', '1', '2019-05-15 11:11:59', '2019-05-15 15:18:43');




ALTER TABLE `index_init`
MODIFY COLUMN `transport_volume`  decimal(10,4) NOT NULL COMMENT '集峰联运-运量' AFTER `finished_order_cnt`;

-- 角色名称用汉字显示
UPDATE `st_platform`.`u_roles` SET `name` = '企业管理员' WHERE `id` = 1;
UPDATE `st_platform`.`u_roles` SET `name` = '普通用户' WHERE `id` = 3;
-- 删除无用的角色
delete from u_roles where id = 4;

-- ----------------------------
-- Table structure for m_baseinfo
-- ----------------------------
DROP TABLE IF EXISTS `m_baseinfo`;
CREATE TABLE `m_baseinfo` (
  `id` int(11) NOT NULL AUTO_INCREMENT COMMENT '主键标识',
  `customer_service_hotline` varchar(255) DEFAULT NULL COMMENT '客服热线',
  `office_phone` varchar(255) DEFAULT NULL,
  `e_mail` varchar(255) DEFAULT NULL COMMENT '邮箱地址',
  `address` varchar(255) DEFAULT NULL COMMENT '地址',
  `status` int(11) DEFAULT NULL COMMENT '状态标识',
  `create_id` bigint(20) DEFAULT NULL,
  `create_time` datetime DEFAULT NULL,
  `update_id` bigint(20) DEFAULT NULL,
  `update_time` datetime DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8mb4;

-- ----------------------------
-- Records of m_baseinfo
-- ----------------------------
INSERT INTO `m_baseinfo` VALUES ('1', '15208235108', '028-85988267', '1155542@qq.com', '成都市高新区天泰路112号12层', '0', null, null, '18', '2019-05-21 17:14:44');


-- ----------------------------
-- 解决只选择编辑权限导致用户无法登录的问题
-- ----------------------------
INSERT INTO `st_platform`.`m_role_role`(`parent_role_id`, `child_role_id`) VALUES (6, 5);
INSERT INTO `st_platform`.`m_role_role`(`parent_role_id`, `child_role_id`) VALUES (9, 8);
INSERT INTO `st_platform`.`m_role_role`(`parent_role_id`, `child_role_id`) VALUES (12, 11);
INSERT INTO `st_platform`.`m_role_role`(`parent_role_id`, `child_role_id`) VALUES (15, 14);
INSERT INTO `st_platform`.`m_role_role`(`parent_role_id`, `child_role_id`) VALUES (18, 17);
INSERT INTO `st_platform`.`m_role_role`(`parent_role_id`, `child_role_id`) VALUES (21, 20);
INSERT INTO `st_platform`.`m_role_role`(`parent_role_id`, `child_role_id`) VALUES (24, 23);
INSERT INTO `st_platform`.`m_role_role`(`parent_role_id`, `child_role_id`) VALUES (27, 26);


