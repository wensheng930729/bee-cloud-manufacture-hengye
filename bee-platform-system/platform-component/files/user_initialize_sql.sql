/*
 Navicat Premium Data Transfer

 Source Server         : 192.168.3.78
 Source Server Type    : MySQL
 Source Server Version : 80015
 Source Host           : 192.168.3.78:3306
 Source Schema         : user_cloudmanufacture

 Target Server Type    : MySQL
 Target Server Version : 80015
 File Encoding         : 65001

 Date: 13/10/2019 17:20:57
*/

SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

-- ----------------------------
-- Table structure for auth_app_version
-- ----------------------------
DROP TABLE IF EXISTS `auth_app_version`;
CREATE TABLE `auth_app_version`  (
  `id` int(10) NOT NULL AUTO_INCREMENT COMMENT '主键',
  `app_type` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT 'app类型',
  `download_addr` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT 'app最新版本下载地址',
  `latest_version` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '最新版本号',
  `spare_addr` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '备用最新下载地址',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB AUTO_INCREMENT = 3 CHARACTER SET = utf8mb4 COLLATE = utf8mb4_0900_ai_ci COMMENT = 'app版本号相关代码' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of auth_app_version
-- ----------------------------
INSERT INTO `auth_app_version` VALUES (1, 'android', 'https://obs-fe91.obs.cn-south-1.myhuaweicloud.com:443/F%3A%2FImgTest%2F8898796862484da693cbfbd7098285d7.apk', '1.0.0', NULL);
INSERT INTO `auth_app_version` VALUES (2, 'ios', 'https://www.baidu.com/', '1.0.0', NULL);

-- ----------------------------
-- Table structure for auth_enterprise
-- ----------------------------
DROP TABLE IF EXISTS `auth_enterprise`;
CREATE TABLE `auth_enterprise`  (
  `id` int(11) NOT NULL AUTO_INCREMENT COMMENT '主键id',
  `enterprise_no` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '公司编号',
  `name` varchar(60) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NOT NULL COMMENT '公司全称',
  `simple_name` varchar(60) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '公司简称',
  `admin` varchar(32) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '企业管理员',
  `pid` int(11) DEFAULT 0 COMMENT '上级公司',
  `type` int(2) DEFAULT NULL COMMENT '企业类型 1企业 2物流商(兼容旧版本)',
  `contact` varchar(13) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '公司联系方式',
  `linkman` varchar(20) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '指定联系人',
  `street` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '详细街道地址（(兼容旧版本)）',
  `regionid` int(6) DEFAULT NULL COMMENT '县级地区id',
  `address` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '地址',
  `industry` tinyint(2) DEFAULT NULL COMMENT '所属行业',
  `status` tinyint(2) NOT NULL DEFAULT 1 COMMENT '状态：1启动 0禁用',
  `deleted` tinyint(2) NOT NULL DEFAULT 0 COMMENT '是否删除 0未删除 1删除',
  `child_num` int(11) DEFAULT NULL COMMENT '可以添加子公司数目',
  `reason` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '删除原因',
  `operate_id` int(11) DEFAULT NULL COMMENT '操作者id（记录企业的添加者）',
  `create_time` datetime(0) DEFAULT NULL COMMENT '创建时间',
  `update_time` datetime(0) DEFAULT NULL COMMENT '更新时间',
  `deleted_time` datetime(0) DEFAULT NULL COMMENT '删除时间',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB AUTO_INCREMENT = 1501 CHARACTER SET = utf8mb4 COLLATE = utf8mb4_0900_ai_ci COMMENT = '企业表' ROW_FORMAT = Compact;

-- ----------------------------
-- Records of auth_enterprise
-- ----------------------------
INSERT INTO `auth_enterprise` VALUES (1500, '201908185001', '四川明达科技云服务有限公司', '明达科技', '18581990837', 0, 1, '028-111111', '李俊杨', '成都', 13, '成都', 1, 1, 0, 0, NULL, 0, '2019-09-18 11:46:56', '2019-09-18 11:46:59', NULL);

-- ----------------------------
-- Table structure for auth_factory
-- ----------------------------
DROP TABLE IF EXISTS `auth_factory`;
CREATE TABLE `auth_factory`  (
  `factory_id` int(10) NOT NULL AUTO_INCREMENT COMMENT '工厂id',
  `factory_name` varchar(60) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '工厂名称',
  `enterprise_id` int(10) DEFAULT NULL COMMENT '企业id',
  PRIMARY KEY (`factory_id`) USING BTREE,
  INDEX `index_enterprise`(`enterprise_id`) USING BTREE
) ENGINE = InnoDB AUTO_INCREMENT = 2 CHARACTER SET = utf8mb4 COLLATE = utf8mb4_0900_ai_ci COMMENT = '工厂类' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of auth_factory
-- ----------------------------
INSERT INTO `auth_factory` VALUES (1, '工厂1', 1500);

-- ----------------------------
-- Table structure for auth_interface
-- ----------------------------
DROP TABLE IF EXISTS `auth_interface`;
CREATE TABLE `auth_interface`  (
  `id` int(11) UNSIGNED NOT NULL AUTO_INCREMENT COMMENT '资源id',
  `name` varchar(70) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NOT NULL COMMENT '资源名称',
  `type` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NOT NULL COMMENT '资源类型',
  `sub_sys` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '子系统标识',
  `order_num` int(11) DEFAULT NULL COMMENT '排序',
  `bee_router` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '资源路由',
  `url` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NOT NULL COMMENT '资源地址',
  `status` int(2) NOT NULL DEFAULT 1 COMMENT '是否启用：1启用 0禁用 ',
  `create_time` datetime(0) NOT NULL COMMENT '创建时间',
  `update_time` datetime(0) DEFAULT NULL COMMENT '更新时间',
  `deleted` tinyint(2) NOT NULL DEFAULT 0 COMMENT '是否删除：1是 0否',
  PRIMARY KEY (`id`) USING BTREE,
  INDEX `idx_name`(`name`) USING BTREE
) ENGINE = InnoDB AUTO_INCREMENT = 906 CHARACTER SET = utf8mb4 COLLATE = utf8mb4_0900_ai_ci ROW_FORMAT = Compact;

-- ----------------------------
-- Table structure for auth_platform_user
-- ----------------------------
DROP TABLE IF EXISTS `auth_platform_user`;
CREATE TABLE `auth_platform_user`  (
  `id` int(11) UNSIGNED NOT NULL AUTO_INCREMENT COMMENT '用户id',
  `beesrv_id` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '业务id',
  `phone` varchar(20) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NOT NULL COMMENT '手机号',
  `name` varchar(60) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NOT NULL COMMENT '姓名',
  `username` varchar(60) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NOT NULL COMMENT '用户账号：现在是手机号',
  `nickname` varchar(60) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '用户名',
  `password` varchar(200) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '密码',
  `head` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '头像',
  `email` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '邮箱',
  `qq` varchar(20) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT 'qq',
  `region_id` varchar(5) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '县级地区id',
  `address` varchar(20) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '详细地址',
  `fixtel` varchar(20) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '固话',
  `sys_token` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '登录凭证',
  `expires_in` datetime(0) DEFAULT NULL COMMENT 'token失效时间',
  `current_enterprise_id` int(11) DEFAULT NULL COMMENT '当前登录企业id',
  `status` int(2) NOT NULL DEFAULT 1 COMMENT '是否启用：1启用 0禁用 ',
  `active_type` int(2) DEFAULT NULL COMMENT '用户激活类型：0平台注册 1平台添加',
  `user_type` int(2) DEFAULT NULL COMMENT '用户类型：0中台用户 1后台用户 2普通用户',
  `create_time` datetime(0) NOT NULL COMMENT '创建时间',
  `update_time` datetime(0) DEFAULT NULL COMMENT '更新时间',
  `update_user` int(11) DEFAULT NULL COMMENT '修改人id',
  `deleted` tinyint(2) NOT NULL DEFAULT 0 COMMENT '是否删除：1是 0否',
  `account_description` text CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci COMMENT '账户说明',
  `current_client_id` varchar(60) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '当前登录的客户端唯一标识',
  PRIMARY KEY (`id`) USING BTREE,
  INDEX `idx_username`(`username`) USING BTREE
) ENGINE = InnoDB AUTO_INCREMENT = 1134 CHARACTER SET = utf8mb4 COLLATE = utf8mb4_0900_ai_ci ROW_FORMAT = Compact;

-- ----------------------------
-- Records of auth_platform_user
-- ----------------------------
INSERT INTO `auth_platform_user` VALUES (1001, '', '11111111111', '管理员账号', '11111111111', '管理员账号', '$2a$10$v7/wYK3TWi0OaD67jHZnJOfwqzNq47.1ZwtR/ZxrHmTW8/OAWF1xe', NULL, NULL, '', '13', '成都', NULL, 'sysToken-382c0cec-e919-4304-87c4-1ce7685aa101-uBkOPj', '2019-10-14 14:34:30', 1500, 1, 1, 1, '2019-09-18 11:43:19', '2019-09-25 17:16:46', 1001, 0, '管理员账号', 'string');
INSERT INTO `auth_platform_user` VALUES (1002, NULL, '22222222222', '测试账号1', '22222222222', '测试账号1', '$2a$10$v7/wYK3TWi0OaD67jHZnJOfwqzNq47.1ZwtR/ZxrHmTW8/OAWF1xe', NULL, NULL, '1111111', '1', '成都', NULL, 'sysToken-0b949986-d5fd-4211-9265-d6e527800b55-ZRbb72', '2019-10-14 14:34:30', 1500, 1, 1, 1, '2019-09-26 11:36:35', '2019-09-26 11:36:37', NULL, 0, '测试账号1', '865166027420697');
INSERT INTO `auth_platform_user` VALUES (1133, NULL, '33333333333', '测试账号2', '33333333333', '测试账号2', '$2a$10$GYGJ.rSsYqGZ2tcMzyUWZ.b/NXSefWq6ha086gCk2d8KnL/IkHrMe', NULL, NULL, NULL, NULL, '成都', NULL, 'sysToken-38e298d1-baf6-4895-bbd7-1825d5c63747-BB50Kn', '2019-10-14 14:34:30', 1500, 1, NULL, NULL, '2019-09-26 20:05:43', '2019-09-26 20:05:43', NULL, 0, '测试账号2', NULL);

-- ----------------------------
-- Table structure for auth_platform_user_enterprise
-- ----------------------------
DROP TABLE IF EXISTS `auth_platform_user_enterprise`;
CREATE TABLE `auth_platform_user_enterprise`  (
  `id` int(11) NOT NULL AUTO_INCREMENT COMMENT '主键id',
  `user_id` int(11) NOT NULL COMMENT '用户id',
  `enterprise_id` int(11) NOT NULL COMMENT '企业id',
  `departments_id` int(11) DEFAULT NULL COMMENT '部门id',
  `factory_id` int(11) NOT NULL COMMENT '工厂id',
  `post_id` int(11) DEFAULT NULL COMMENT '职位id',
  `status` tinyint(2) NOT NULL DEFAULT 1 COMMENT '状态：1启动 0禁用',
  `deleted` tinyint(2) NOT NULL DEFAULT 0 COMMENT '是否删除 0未删除 1删除',
  `create_user` int(11) DEFAULT NULL COMMENT '创建人',
  `create_time` datetime(0) NOT NULL COMMENT '创建时间',
  `update_time` datetime(0) DEFAULT NULL COMMENT '更新时间',
  PRIMARY KEY (`id`) USING BTREE,
  INDEX `idx_enterprise_user`(`enterprise_id`, `user_id`) USING BTREE
) ENGINE = InnoDB AUTO_INCREMENT = 3432 CHARACTER SET = utf8mb4 COLLATE = utf8mb4_0900_ai_ci COMMENT = '企业与用户中间表' ROW_FORMAT = Compact;

-- ----------------------------
-- Records of auth_platform_user_enterprise
-- ----------------------------
INSERT INTO `auth_platform_user_enterprise` VALUES (3429, 1001, 1500, NULL, 1, NULL, 1, 0, 0, '2019-09-18 11:49:05', '2019-09-18 11:49:07');
INSERT INTO `auth_platform_user_enterprise` VALUES (3430, 1002, 1500, NULL, 1, NULL, 1, 0, 0, '2019-09-26 11:38:32', '2019-09-26 11:38:34');
INSERT INTO `auth_platform_user_enterprise` VALUES (3431, 1133, 1500, NULL, 1, NULL, 1, 0, 1001, '2019-09-26 20:05:43', '2019-09-26 20:05:43');

-- ----------------------------
-- Table structure for auth_resource
-- ----------------------------
DROP TABLE IF EXISTS `auth_resource`;
CREATE TABLE `auth_resource`  (
  `id` int(10) NOT NULL AUTO_INCREMENT COMMENT '菜单id',
  `pid` int(10) DEFAULT 0 COMMENT '父id',
  `sub_sys` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NOT NULL COMMENT '子系统标识',
  `name` varchar(70) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NOT NULL COMMENT '菜单名称',
  `resource_type` varchar(60) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '菜单类型',
  `icon` varchar(200) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '菜单图标',
  `path` varchar(200) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '菜单url',
  `component` varchar(200) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '菜单com破net',
  `order_num` int(11) DEFAULT NULL COMMENT '菜单序号',
  `is_hide` tinyint(1) DEFAULT NULL COMMENT '是否隐藏0展开1隐藏',
  `position` varchar(60) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '菜单所在位置',
  `show_type` tinyint(1) DEFAULT NULL,
  `deleted` tinyint(1) NOT NULL DEFAULT 0 COMMENT '是否删除0未删除1已删除',
  `create_time` datetime(0) NOT NULL COMMENT '创建时间',
  `update_time` timestamp(0) DEFAULT NULL ON UPDATE CURRENT_TIMESTAMP(0) COMMENT '修改时间',
  PRIMARY KEY (`id`) USING BTREE,
  UNIQUE INDEX `index_position`(`position`) USING BTREE,
  INDEX `idx_name`(`name`) USING BTREE
) ENGINE = InnoDB AUTO_INCREMENT = 1181 CHARACTER SET = utf8mb4 COLLATE = utf8mb4_0900_ai_ci COMMENT = '资源表' ROW_FORMAT = Compact;

-- ----------------------------
-- Records of auth_resource
-- ----------------------------
INSERT INTO `auth_resource` VALUES (1, 0, 'cloud_maf_app', '采购商', 'function', '', 'purchaser', NULL, NULL, 1, '1', NULL, 0, '2019-03-15 00:00:00', '2019-10-10 11:19:22');
INSERT INTO `auth_resource` VALUES (10, 0, 'cloud_maf_app', '物流人员', 'function', NULL, 'logistics', NULL, NULL, 1, '2', NULL, 0, '2019-07-19 08:54:07', '2019-10-10 11:28:12');
INSERT INTO `auth_resource` VALUES (20, 0, 'cloud_maf_app', '取样人员', 'function', NULL, 'samplePerson', NULL, NULL, 1, '3', NULL, 0, '2019-09-25 17:40:07', '2019-10-10 11:28:13');
INSERT INTO `auth_resource` VALUES (30, 0, 'cloud_maf_app', '化验人员', 'function', NULL, 'assayPerson', NULL, NULL, 1, '4', NULL, 0, '2019-09-25 09:51:49', '2019-10-10 11:28:14');
INSERT INTO `auth_resource` VALUES (40, 0, 'cloud_maf_app', '物流承运商', 'function', NULL, 'transLogistics', NULL, NULL, 1, '5', NULL, 0, '2019-09-25 17:38:22', '2019-10-10 11:28:16');
INSERT INTO `auth_resource` VALUES (50, 0, 'cloud_maf_app', '磅房人员', 'function', NULL, 'weighingPerson', NULL, NULL, 1, '6', NULL, 0, '2019-09-25 17:39:22', '2019-10-10 11:28:17');
INSERT INTO `auth_resource` VALUES (60, 0, 'cloud_maf_app', '仓储人员', 'function', NULL, 'warehousePerson', NULL, NULL, 1, '7', NULL, 0, '2019-10-09 15:14:13', '2019-10-10 11:28:18');
INSERT INTO `auth_resource` VALUES (70, 0, 'cloud_maf_app', '质检主管', 'function', NULL, 'detectionManager', NULL, NULL, 1, '8', NULL, 0, '2019-09-30 11:01:59', '2019-10-10 11:28:20');
INSERT INTO `auth_resource` VALUES (80, 0, 'cloud_maf_app', '工艺主管', 'function', NULL, 'crafts', NULL, NULL, 1, '9', NULL, 0, '2019-10-09 15:16:51', '2019-10-10 11:28:27');
INSERT INTO `auth_resource` VALUES (90, 0, 'cloud_maf_app', '配料人员', 'function', NULL, 'batching', NULL, NULL, 1, '10', NULL, 0, '2019-09-30 11:04:54', '2019-10-10 11:28:29');
INSERT INTO `auth_resource` VALUES (100, 0, 'cloud_maf_app', '矿热炉记录', 'function', NULL, 'RKEF', NULL, NULL, 1, '11', NULL, 0, '2019-09-30 11:06:53', '2019-10-10 11:28:30');
INSERT INTO `auth_resource` VALUES (110, 0, 'cloud_maf_app', '成品装袋', 'function', NULL, 'bag', NULL, NULL, 1, '12', NULL, 0, '2019-09-30 11:09:21', '2019-10-10 11:28:31');
INSERT INTO `auth_resource` VALUES (120, 0, 'cloud_maf_app', '设备巡检', 'function', NULL, 'inspect', NULL, NULL, 1, '13', NULL, 0, '2019-09-30 11:10:53', '2019-10-10 11:28:33');
INSERT INTO `auth_resource` VALUES (130, 0, 'cloud_maf_app', '车次确认', 'function', NULL, 'trainGoods', NULL, NULL, 1, '14', NULL, 0, '2019-10-09 15:18:32', '2019-10-10 11:28:35');
INSERT INTO `auth_resource` VALUES (140, 0, 'cloud_maf_app', '供销结算', 'function', NULL, 'settlement', NULL, NULL, 1, '15', NULL, 0, '2019-10-10 11:23:45', '2019-10-10 11:28:36');
INSERT INTO `auth_resource` VALUES (150, 0, 'cloud_maf_app', '销售人员', 'function', NULL, 'saleman', NULL, NULL, 1, '16', NULL, 0, '2019-10-10 11:24:16', '2019-10-10 11:28:37');
INSERT INTO `auth_resource` VALUES (1000, 0, 'cloud_maf_web', '工厂系统配置', 'menu', NULL, 'web', NULL, NULL, 1, '100', NULL, 0, '2019-03-15 00:00:00', '2019-10-10 11:28:40');
INSERT INTO `auth_resource` VALUES (1010, 1000, 'cloud_maf_web', '设备管理', 'menu', NULL, 'web', NULL, NULL, 1, '100-1', NULL, 0, '2019-07-19 08:54:07', '2019-10-10 11:28:43');
INSERT INTO `auth_resource` VALUES (1020, 1010, 'cloud_maf_web', '电表管理', 'function', NULL, 'web', NULL, NULL, 0, '100-1-1', NULL, 0, '2019-07-19 08:54:07', '2019-10-10 11:28:45');
INSERT INTO `auth_resource` VALUES (1030, 1020, 'cloud_maf_web', '列表查询', 'page', NULL, 'web', NULL, NULL, 0, '100-1-1-1', NULL, 0, '2019-07-19 08:54:07', '2019-10-10 11:28:47');
INSERT INTO `auth_resource` VALUES (1040, 1020, 'cloud_maf_web', '编辑页面', 'page', NULL, 'web', NULL, NULL, 0, '100-1-1-2', NULL, 0, '2019-07-19 08:54:07', '2019-10-10 11:28:49');
INSERT INTO `auth_resource` VALUES (1050, 1010, 'cloud_maf_web', '电价管理', 'function', NULL, 'web', NULL, NULL, 0, '100-1-2', NULL, 0, '2019-09-24 16:04:36', '2019-10-10 11:28:53');
INSERT INTO `auth_resource` VALUES (1060, 1050, 'cloud_maf_web', '列表查询', 'page', NULL, 'web', NULL, NULL, 0, '100-1-2-1', NULL, 0, '2019-09-25 09:51:49', '2019-10-10 11:28:55');
INSERT INTO `auth_resource` VALUES (1070, 1050, 'cloud_maf_web', '新增', 'page', NULL, 'web', NULL, NULL, 0, '100-1-2-2', NULL, 0, '2019-09-25 09:53:32', '2019-10-10 11:28:57');
INSERT INTO `auth_resource` VALUES (1080, 1000, 'cloud_maf_web', '产品管理', 'menu', NULL, 'web', NULL, NULL, 1, '100-2', NULL, 0, '2019-09-25 17:37:37', '2019-10-10 11:28:59');
INSERT INTO `auth_resource` VALUES (1090, 1080, 'cloud_maf_web', '新增产品', 'function', NULL, 'web', NULL, NULL, 0, '100-2-1', NULL, 0, '2019-09-25 17:38:22', '2019-10-10 11:29:01');
INSERT INTO `auth_resource` VALUES (1100, 1080, 'cloud_maf_web', '化验配置', 'function', NULL, 'web', NULL, NULL, 0, '100-2-2', NULL, 0, '2019-09-25 09:53:32', '2019-10-10 11:29:03');
INSERT INTO `auth_resource` VALUES (1110, 1080, 'cloud_maf_web', '产品编辑', 'function', NULL, 'web', NULL, NULL, 0, '100-2-3', NULL, 0, '2019-09-25 17:37:37', '2019-10-10 11:29:06');
INSERT INTO `auth_resource` VALUES (1120, 1000, 'cloud_maf_web', '权限设置', 'menu', NULL, 'web', NULL, NULL, 1, '100-3', NULL, 0, '2019-09-25 17:39:22', '2019-10-10 11:29:08');
INSERT INTO `auth_resource` VALUES (1130, 1120, 'cloud_maf_web', '人员管理', 'function', NULL, 'web', NULL, NULL, 0, '100-3-1', NULL, 0, '2019-09-25 09:53:32', '2019-10-10 11:29:10');
INSERT INTO `auth_resource` VALUES (1140, 1130, 'cloud_maf_web', '列表查询', 'page', NULL, 'web', NULL, NULL, 0, '100-3-1-1', NULL, 0, '2019-09-25 17:37:37', '2019-10-10 11:29:11');
INSERT INTO `auth_resource` VALUES (1150, 1130, 'cloud_maf_web', '新增人员', 'page', NULL, 'web', NULL, NULL, 0, '100-3-1-2', NULL, 0, '2019-09-25 17:40:07', '2019-10-10 11:29:13');
INSERT INTO `auth_resource` VALUES (1160, 1120, 'cloud_maf_web', '角色管理', 'function', NULL, 'web', NULL, NULL, 0, '100-3-2', NULL, 0, '2019-09-25 09:53:32', '2019-10-10 11:29:15');
INSERT INTO `auth_resource` VALUES (1170, 1160, 'cloud_maf_web', '列表查询', 'page', NULL, 'web', NULL, NULL, 0, '100-3-2-1', NULL, 0, '2019-09-25 17:37:37', '2019-10-10 11:29:17');
INSERT INTO `auth_resource` VALUES (1180, 1160, 'cloud_maf_web', '权限详细', 'page', NULL, 'web', NULL, NULL, 0, '100-3-2-2', NULL, 0, '2019-03-15 00:00:00', '2019-10-10 11:29:18');

-- ----------------------------
-- Table structure for auth_role
-- ----------------------------
DROP TABLE IF EXISTS `auth_role`;
CREATE TABLE `auth_role`  (
  `id` int(10) NOT NULL AUTO_INCREMENT COMMENT '角色id',
  `role_name` varchar(32) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NOT NULL COMMENT '角色名称',
  `role_type` varchar(32) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '角色类型(base基础类角色 application应用类角色 function功能类角色 custom用户自定义角色 enterprise_admin企业管理员 super_admin超级管理员other其它)',
  `level` tinyint(2) DEFAULT NULL COMMENT '角色级别',
  `sub_sys` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '子系统标识',
  `describe` varchar(200) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '描述',
  `deleted` tinyint(1) NOT NULL COMMENT '是否删除0未删除1已删除',
  `create_user` int(11) NOT NULL COMMENT '创建人',
  `create_time` datetime(0) NOT NULL COMMENT '创建时间',
  `update_time` datetime(0) DEFAULT NULL COMMENT '修改时间',
  `update_user` int(11) DEFAULT NULL COMMENT '修改人id',
  PRIMARY KEY (`id`) USING BTREE,
  INDEX `index_role_type`(`role_type`) USING BTREE,
  INDEX `index_level`(`level`) USING BTREE
) ENGINE = InnoDB AUTO_INCREMENT = 102 CHARACTER SET = utf8mb4 COLLATE = utf8mb4_0900_ai_ci COMMENT = '角色表' ROW_FORMAT = Compact;

-- ----------------------------
-- Records of auth_role
-- ----------------------------
INSERT INTO `auth_role` VALUES (1, '测试角色', 'super_admin', NULL, NULL, '测试角色', 0, 1, '2019-09-24 10:18:12', '2019-09-24 10:18:17', NULL);
INSERT INTO `auth_role` VALUES (2, '测2', 'base', NULL, NULL, '测试角色2', 0, 1, '2019-09-25 17:06:19', '2019-09-26 19:47:12', 1001);
INSERT INTO `auth_role` VALUES (100, '测3', 'base', NULL, NULL, '测试角色3', 1, 1001, '2019-09-26 19:49:06', '2019-09-26 19:52:17', NULL);
INSERT INTO `auth_role` VALUES (101, '测4', 'base', NULL, NULL, '测试角色4', 0, 1001, '2019-09-26 19:53:01', '2019-09-26 19:55:18', 1001);

-- ----------------------------
-- Table structure for auth_role_interface
-- ----------------------------
DROP TABLE IF EXISTS `auth_role_interface`;
CREATE TABLE `auth_role_interface`  (
  `id` int(11) UNSIGNED NOT NULL AUTO_INCREMENT COMMENT '主键id',
  `role_id` int(11) UNSIGNED NOT NULL COMMENT '角色id',
  `interface_id` int(11) UNSIGNED NOT NULL COMMENT '接口id',
  `status` int(2) NOT NULL DEFAULT 1 COMMENT '是否启用：1启用 0禁用 ',
  `order_num_id` int(11) UNSIGNED DEFAULT NULL COMMENT '该角色下接口序号',
  `create_user` int(11) UNSIGNED NOT NULL COMMENT '创建人',
  `create_time` datetime(0) NOT NULL COMMENT '创建时间',
  `update_time` datetime(0) DEFAULT NULL COMMENT '更新时间',
  `deleted` tinyint(2) NOT NULL DEFAULT 0 COMMENT '是否删除：1是 0否',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB AUTO_INCREMENT = 1 CHARACTER SET = utf8mb4 COLLATE = utf8mb4_0900_ai_ci COMMENT = '接口角色（基础）表' ROW_FORMAT = Compact;

-- ----------------------------
-- Table structure for auth_role_resource
-- ----------------------------
DROP TABLE IF EXISTS `auth_role_resource`;
CREATE TABLE `auth_role_resource`  (
  `id` int(10) NOT NULL AUTO_INCREMENT COMMENT '主键id',
  `role_id` int(10) NOT NULL COMMENT '角色id',
  `resource_id` int(10) NOT NULL COMMENT '菜单id',
  `order_num` int(11) DEFAULT NULL COMMENT '菜单序号',
  `deleted` tinyint(1) NOT NULL DEFAULT 0 COMMENT '是否删除0未删除1已删除',
  `create_user` int(11) NOT NULL COMMENT '创建人',
  `create_time` datetime(0) NOT NULL COMMENT '创建时间',
  `update_time` datetime(0) DEFAULT NULL COMMENT '修改时间',
  PRIMARY KEY (`id`) USING BTREE,
  INDEX `idx_resource_id`(`resource_id`) USING BTREE,
  INDEX `idx_role_id`(`role_id`) USING BTREE
) ENGINE = InnoDB AUTO_INCREMENT = 1801 CHARACTER SET = utf8mb4 COLLATE = utf8mb4_0900_ai_ci COMMENT = '资源角色(功能)表' ROW_FORMAT = Compact;

-- ----------------------------
-- Records of auth_role_resource
-- ----------------------------
INSERT INTO `auth_role_resource` VALUES (1655, 1, 1, NULL, 0, 1, '2019-09-24 15:44:26', '2019-09-24 15:44:29');
INSERT INTO `auth_role_resource` VALUES (1656, 1, 10, NULL, 0, 1, '2019-09-24 15:44:59', '2019-09-24 15:45:02');
INSERT INTO `auth_role_resource` VALUES (1657, 1, 20, NULL, 0, 1, '2019-09-24 15:46:22', '2019-09-24 15:46:24');
INSERT INTO `auth_role_resource` VALUES (1658, 1, 30, NULL, 0, 1, '2019-09-24 15:46:42', '2019-09-24 15:46:44');
INSERT INTO `auth_role_resource` VALUES (1659, 1, 40, NULL, 0, 1, '2019-09-25 10:50:29', '2019-09-25 10:50:31');
INSERT INTO `auth_role_resource` VALUES (1660, 1, 50, NULL, 0, 1, '2019-09-25 10:50:39', '2019-09-25 10:50:41');
INSERT INTO `auth_role_resource` VALUES (1661, 1, 60, NULL, 0, 1, '2019-09-25 10:50:48', '2019-09-25 10:50:49');
INSERT INTO `auth_role_resource` VALUES (1662, 1, 70, NULL, 0, 1, '2019-09-25 10:50:59', '2019-09-25 10:51:01');
INSERT INTO `auth_role_resource` VALUES (1663, 1, 80, NULL, 0, 1, '2019-09-24 15:44:26', '2019-09-24 15:44:29');
INSERT INTO `auth_role_resource` VALUES (1664, 1, 90, NULL, 0, 1, '2019-09-24 15:44:59', '2019-09-24 15:45:02');
INSERT INTO `auth_role_resource` VALUES (1665, 1, 100, NULL, 0, 1, '2019-09-24 15:46:22', '2019-09-24 15:46:24');
INSERT INTO `auth_role_resource` VALUES (1666, 1, 110, NULL, 0, 1, '2019-09-24 15:46:42', '2019-09-24 15:46:44');
INSERT INTO `auth_role_resource` VALUES (1667, 1, 120, NULL, 0, 1, '2019-09-25 10:50:29', '2019-09-25 10:50:31');
INSERT INTO `auth_role_resource` VALUES (1668, 1, 130, NULL, 0, 1, '2019-09-25 10:50:39', '2019-09-25 10:50:41');
INSERT INTO `auth_role_resource` VALUES (1669, 1, 140, NULL, 0, 1, '2019-09-25 10:50:48', '2019-09-25 10:50:49');
INSERT INTO `auth_role_resource` VALUES (1670, 1, 150, NULL, 0, 1, '2019-09-25 10:50:59', '2019-09-25 10:51:01');
INSERT INTO `auth_role_resource` VALUES (1673, 1, 1000, NULL, 0, 1, '2019-09-24 15:44:26', '2019-09-24 15:44:29');
INSERT INTO `auth_role_resource` VALUES (1674, 1, 1010, NULL, 0, 1, '2019-09-24 15:44:59', '2019-09-24 15:45:02');
INSERT INTO `auth_role_resource` VALUES (1675, 1, 1020, NULL, 0, 1, '2019-09-24 15:46:22', '2019-09-24 15:46:24');
INSERT INTO `auth_role_resource` VALUES (1676, 1, 1030, NULL, 0, 1, '2019-09-24 15:46:42', '2019-09-24 15:46:44');
INSERT INTO `auth_role_resource` VALUES (1677, 1, 1040, NULL, 0, 1, '2019-09-25 10:50:29', '2019-09-25 10:50:31');
INSERT INTO `auth_role_resource` VALUES (1678, 1, 1050, NULL, 0, 1, '2019-09-25 10:50:39', '2019-09-25 10:50:41');
INSERT INTO `auth_role_resource` VALUES (1679, 1, 1060, NULL, 0, 1, '2019-09-25 10:50:48', '2019-09-25 10:50:49');
INSERT INTO `auth_role_resource` VALUES (1680, 1, 1070, NULL, 0, 1, '2019-09-25 10:50:59', '2019-09-25 10:51:01');
INSERT INTO `auth_role_resource` VALUES (1681, 1, 1080, NULL, 0, 1, '2019-09-24 15:44:26', '2019-09-24 15:44:29');
INSERT INTO `auth_role_resource` VALUES (1682, 1, 1090, NULL, 0, 1, '2019-09-24 15:44:59', '2019-09-24 15:45:02');
INSERT INTO `auth_role_resource` VALUES (1683, 1, 1100, NULL, 0, 1, '2019-09-24 15:46:22', '2019-09-24 15:46:24');
INSERT INTO `auth_role_resource` VALUES (1684, 1, 1110, NULL, 0, 1, '2019-09-24 15:46:42', '2019-09-24 15:46:44');
INSERT INTO `auth_role_resource` VALUES (1685, 1, 1120, NULL, 0, 1, '2019-09-25 10:50:29', '2019-09-25 10:50:31');
INSERT INTO `auth_role_resource` VALUES (1686, 1, 1130, NULL, 0, 1, '2019-09-25 10:50:39', '2019-09-25 10:50:41');
INSERT INTO `auth_role_resource` VALUES (1687, 1, 1140, NULL, 0, 1, '2019-09-25 10:50:48', '2019-09-25 10:50:49');
INSERT INTO `auth_role_resource` VALUES (1688, 1, 1150, NULL, 0, 1, '2019-09-25 10:50:59', '2019-09-25 10:51:01');
INSERT INTO `auth_role_resource` VALUES (1689, 1, 1160, NULL, 0, 1, '2019-09-24 15:44:26', '2019-09-24 15:44:29');
INSERT INTO `auth_role_resource` VALUES (1690, 1, 1170, NULL, 0, 1, '2019-09-24 15:44:59', '2019-09-24 15:45:02');
INSERT INTO `auth_role_resource` VALUES (1691, 1, 1180, NULL, 0, 1, '2019-09-24 15:44:59', '2019-09-24 15:45:02');
INSERT INTO `auth_role_resource` VALUES (1692, 2, 1, NULL, 0, 1001, '2019-09-26 19:47:12', NULL);
INSERT INTO `auth_role_resource` VALUES (1693, 2, 10, NULL, 0, 1001, '2019-09-26 19:47:12', NULL);
INSERT INTO `auth_role_resource` VALUES (1694, 2, 20, NULL, 0, 1001, '2019-09-26 19:47:12', NULL);
INSERT INTO `auth_role_resource` VALUES (1695, 2, 30, NULL, 0, 1001, '2019-09-26 19:47:12', NULL);
INSERT INTO `auth_role_resource` VALUES (1696, 2, 50, NULL, 0, 1001, '2019-09-26 19:47:12', NULL);
INSERT INTO `auth_role_resource` VALUES (1697, 2, 60, NULL, 0, 1001, '2019-09-26 19:47:12', NULL);
INSERT INTO `auth_role_resource` VALUES (1712, 101, 130, NULL, 0, 1001, '2019-09-26 19:55:18', NULL);
INSERT INTO `auth_role_resource` VALUES (1713, 101, 140, NULL, 0, 1001, '2019-09-26 19:55:18', NULL);
INSERT INTO `auth_role_resource` VALUES (1714, 101, 150, NULL, 0, 1001, '2019-09-26 19:55:18', NULL);
INSERT INTO `auth_role_resource` VALUES (1715, 101, 1000, NULL, 0, 1001, '2019-09-26 19:55:18', NULL);
INSERT INTO `auth_role_resource` VALUES (1716, 101, 1010, NULL, 0, 1001, '2019-09-26 19:55:18', NULL);
INSERT INTO `auth_role_resource` VALUES (1717, 101, 1020, NULL, 0, 1001, '2019-09-26 19:55:18', NULL);
INSERT INTO `auth_role_resource` VALUES (1718, 101, 1030, NULL, 0, 1001, '2019-09-26 19:55:18', NULL);
INSERT INTO `auth_role_resource` VALUES (1719, 101, 1040, NULL, 0, 1001, '2019-09-26 19:55:18', NULL);

-- ----------------------------
-- Table structure for auth_user_role
-- ----------------------------
DROP TABLE IF EXISTS `auth_user_role`;
CREATE TABLE `auth_user_role`  (
  `id` int(11) NOT NULL AUTO_INCREMENT COMMENT '主键id',
  `user_id` int(11) NOT NULL COMMENT '用户id',
  `role_id` int(11) NOT NULL COMMENT '角色id',
  `enterprise_id` int(11) NOT NULL DEFAULT 0 COMMENT '企业id',
  `pid` int(11) NOT NULL DEFAULT 0 COMMENT '角色父id',
  `status` int(1) UNSIGNED ZEROFILL DEFAULT 1 COMMENT '是否启用：1启用，0禁用',
  `level` int(11) DEFAULT 0 COMMENT '该用户下角色级别：与角色表的level对应',
  `role_type` varchar(20) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '该用户下角色或功能的自定义分类，基于level字段的一个分类，用于分类展示',
  `order_num` int(11) DEFAULT NULL COMMENT '该用户下角色的序号',
  `create_user` int(11) DEFAULT NULL COMMENT '创建人id',
  `create_time` datetime(0) DEFAULT NULL COMMENT '创建时间',
  `update_time` datetime(0) DEFAULT NULL COMMENT '更新时间',
  `deleted` tinyint(2) UNSIGNED DEFAULT 0 COMMENT '逻辑删除，1-是，0-否',
  `flag` tinyint(1) NOT NULL DEFAULT 0 COMMENT '标识 1 勾选 0未勾选',
  PRIMARY KEY (`id`) USING BTREE,
  INDEX `index_user_id`(`user_id`) USING BTREE,
  INDEX `index_role_id`(`role_id`) USING BTREE,
  INDEX `index_eId`(`enterprise_id`) USING BTREE,
  INDEX `index_role_type`(`role_type`) USING BTREE
) ENGINE = InnoDB AUTO_INCREMENT = 159749 CHARACTER SET = utf8mb4 COLLATE = utf8mb4_0900_ai_ci COMMENT = '用户与角色/功能/应用的关联表' ROW_FORMAT = Compact;

-- ----------------------------
-- Records of auth_user_role
-- ----------------------------
INSERT INTO `auth_user_role` VALUES (159746, 1001, 1, 1500, 0, 1, 0, 'super_admin', NULL, 0, '2019-09-18 11:50:15', '2019-09-18 11:50:17', 0, 0);
INSERT INTO `auth_user_role` VALUES (159747, 1002, 1, 1500, 0, 1, 0, 'super_admin', NULL, 0, '2019-09-26 11:39:10', '2019-09-26 11:39:12', 0, 0);
INSERT INTO `auth_user_role` VALUES (159748, 1133, 101, 1500, 0, 1, 0, NULL, NULL, 1001, '2019-09-26 20:05:43', '2019-09-26 20:05:43', 0, 0);

-- ----------------------------
-- Table structure for t_config
-- ----------------------------
DROP TABLE IF EXISTS `t_config`;
CREATE TABLE `t_config`  (
  `id` int(10) UNSIGNED NOT NULL AUTO_INCREMENT COMMENT '主键',
  `config_key` varchar(60) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '配置的键',
  `config_value` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '该键对应的值',
  `config_desc` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '该项配置的描述信息',
  `status` varchar(2) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT '1' COMMENT '是否启动该配置：1->启用,0->禁用',
  `create_time` datetime(0) DEFAULT NULL COMMENT '创建时间',
  `update_time` timestamp(0) NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP(0) COMMENT '更新时间',
  PRIMARY KEY (`id`) USING BTREE,
  UNIQUE INDEX `uniq_omsconfig_key`(`config_key`) USING BTREE COMMENT '业务配置config_key的唯一索引',
  INDEX `idx_beetradeconfig_status`(`status`) USING BTREE COMMENT '索引业务配置状态'
) ENGINE = InnoDB AUTO_INCREMENT = 40 CHARACTER SET = utf8mb4 COLLATE = utf8mb4_0900_ai_ci COMMENT = '公共业务配置表' ROW_FORMAT = Compact;

-- ----------------------------
-- Records of t_config
-- ----------------------------
INSERT INTO `t_config` VALUES (13, 'bee_trade_user_auth_switch', '1', '权限校验系统，默认不拦截', '1', '2018-12-10 16:10:46', '2019-02-28 19:25:50');
INSERT INTO `t_config` VALUES (14, 'platform_token_expires_in', '86400', 'Token 在redis在的失效时间', '1', '2018-12-16 16:09:56', '2019-10-13 17:20:22');
INSERT INTO `t_config` VALUES (20, 'supplychainfinance_is_enable_syscodecache_v1.0', '1', '是否开启码表缓存的键 0不开启，1开启', '1', '2019-01-21 13:39:35', '2019-01-21 13:39:35');
INSERT INTO `t_config` VALUES (22, 'test_account_login', '0', '是否允许测试账号登陆的开关。0不允许，1允许', '1', '2019-02-27 18:25:42', '2019-03-25 21:22:25');
INSERT INTO `t_config` VALUES (23, 'logout_switch', '1', '退出登录时清除缓存的开关，0.关闭，1打开', '1', '2019-04-04 04:54:51', '2019-04-04 17:54:51');
INSERT INTO `t_config` VALUES (31, 'permission_data_switch', '0', '新老权限系统用户,企业等相关数据迁移的开关，0关闭，1打开', '1', '2019-05-24 11:48:30', '2019-06-15 22:40:29');
INSERT INTO `t_config` VALUES (32, 'initial_company_num', '100', '可添加子公司数量', '1', '2019-05-27 11:18:30', '2019-05-27 11:20:08');
INSERT INTO `t_config` VALUES (33, 'privilege_intercept', '0', '权限拦截开关', '1', NULL, '2019-05-28 14:04:54');
INSERT INTO `t_config` VALUES (34, 'autogetresource_swich', '0', '根据路由、URL、请求方式自动获取接口资源的开关', '1', '2019-06-08 15:52:56', '2019-06-12 14:15:12');
INSERT INTO `t_config` VALUES (35, 'globale_config_switch', '1', '所有配置是否走缓存的全局开关:1启用缓存,0不走缓存', '1', '2019-06-08 18:48:04', '2019-08-20 11:16:32');
INSERT INTO `t_config` VALUES (36, 'enable_authcheck_swich', '0', '是否启用全局权限校验的开关:1启用,0禁用', '1', '2019-06-08 18:49:19', '2019-06-08 20:14:01');
INSERT INTO `t_config` VALUES (37, 'cmf_config_switch', '1', '所有配置是否走缓存的全局开关:1启用缓存,0不走缓存', '1', '2019-09-25 10:40:59', '2019-09-25 10:40:58');
INSERT INTO `t_config` VALUES (38, 'is_open_redis_switch', '1', '配置表中是否打开redis缓存，0 关闭 1 打开 。默认打开', '1', '2019-09-26 19:26:10', '2019-09-30 11:28:05');

-- ----------------------------
-- Table structure for t_system_code_t
-- ----------------------------
DROP TABLE IF EXISTS `t_system_code_t`;
CREATE TABLE `t_system_code_t`  (
  `id` int(10) UNSIGNED NOT NULL AUTO_INCREMENT COMMENT '主键',
  `sys_group_id` varchar(60) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '组id（不唯一）',
  `sys_code` varchar(100) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '该组id下面的key(唯一)',
  `sys_code_val` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '该码表的这个编码对应的值',
  `sys_code_desc` varchar(100) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '该码值的说明x信息',
  `status` varchar(2) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT '1' COMMENT '是否启用的状态：1->启用，0->禁用',
  `order_num` int(10) UNSIGNED DEFAULT NULL COMMENT '序号',
  `create_time` datetime(0) DEFAULT NULL COMMENT '创建时间',
  `update_time` timestamp(0) DEFAULT NULL ON UPDATE CURRENT_TIMESTAMP(0) COMMENT '更新时间',
  PRIMARY KEY (`id`) USING BTREE,
  INDEX `idx_syscode_groupid`(`sys_group_id`) USING BTREE COMMENT '索引组id',
  INDEX `idx_syscode_code`(`sys_code`) USING BTREE COMMENT '索引码值',
  INDEX `idx_syscode_status`(`status`) USING BTREE COMMENT '索引状态',
  INDEX `idx_syscode_ordernum`(`order_num`) USING BTREE COMMENT '索引序号'
) ENGINE = InnoDB AUTO_INCREMENT = 88 CHARACTER SET = utf8mb4 COLLATE = utf8mb4_0900_ai_ci COMMENT = '系统码表' ROW_FORMAT = Compact;

-- ----------------------------
-- Records of t_system_code_t
-- ----------------------------
INSERT INTO `t_system_code_t` VALUES (1, 'sub_sys_clientid', 'beetrade-v2.0', 'go.beesrv.com', '贸易2.0子系统clientid', '1', 1, '2018-12-16 14:36:26', '2018-12-16 14:37:24');
INSERT INTO `t_system_code_t` VALUES (2, 'sub_sys_clientid', 'ifms-v1.0', 'ifms.beesrv.com', '物联网1.0子系统clientid', '1', 2, '2018-12-16 14:40:58', '2018-12-16 14:41:01');
INSERT INTO `t_system_code_t` VALUES (3, 'sub_sys_clientid', 'platform-v3.0', 'www.beesrv.com', '平台3.0', '1', 3, '2018-12-16 14:42:57', '2018-12-16 14:43:33');
INSERT INTO `t_system_code_t` VALUES (6, 'sub_sys_clientid', 'finance-v1.0', NULL, '供应链金融平台', '1', 6, '2018-12-28 11:24:14', '2018-12-28 11:24:15');
INSERT INTO `t_system_code_t` VALUES (7, 'sub_sys_clientid', 'admin-v1.0', NULL, '后台管理系统', '1', 7, '2019-02-21 15:51:14', '2019-02-21 15:51:13');
INSERT INTO `t_system_code_t` VALUES (28, 'test_account', '13512345604', NULL, '货代公司_测试账号', '1', NULL, NULL, NULL);
INSERT INTO `t_system_code_t` VALUES (29, 'test_account', '13512345605', NULL, '业务员_测试账号', '1', NULL, NULL, NULL);
INSERT INTO `t_system_code_t` VALUES (30, 'test_account', '13512345606', NULL, '部门经理_测试账号', '1', NULL, NULL, NULL);
INSERT INTO `t_system_code_t` VALUES (31, 'test_account', '13512345607', NULL, '风控专员_测试账号', '1', NULL, NULL, NULL);
INSERT INTO `t_system_code_t` VALUES (32, 'test_account', '13512345608', NULL, '业务风控_测试账号', '1', NULL, NULL, NULL);
INSERT INTO `t_system_code_t` VALUES (33, 'test_account', '13512345609', NULL, '风控负责人_测试账号', '1', NULL, NULL, NULL);
INSERT INTO `t_system_code_t` VALUES (34, 'test_account', '13512345610', NULL, '数据专员_测试账号', '1', NULL, NULL, NULL);
INSERT INTO `t_system_code_t` VALUES (35, 'test_account', '13512345611', NULL, '结算专员_测试账号', '1', NULL, NULL, NULL);
INSERT INTO `t_system_code_t` VALUES (36, 'test_account', '13512345612', NULL, '结算复核人_测试账号', '1', NULL, NULL, NULL);
INSERT INTO `t_system_code_t` VALUES (37, 'test_account', '13512345613', NULL, '结算负责人_测试账号', '1', NULL, NULL, NULL);
INSERT INTO `t_system_code_t` VALUES (38, 'test_account', '13512345614', NULL, '法务_测试账号', '1', NULL, NULL, NULL);
INSERT INTO `t_system_code_t` VALUES (39, 'test_account', '13512345615', NULL, '决策委员会成员_测试账号', '1', NULL, NULL, NULL);
INSERT INTO `t_system_code_t` VALUES (40, 'test_account', '13512345616', NULL, '决策委员会成员_测试账号', '1', NULL, NULL, NULL);
INSERT INTO `t_system_code_t` VALUES (41, 'test_account', '13512345617', NULL, '决策委员会成员_测试账号', '1', NULL, NULL, NULL);
INSERT INTO `t_system_code_t` VALUES (42, 'test_account', '13512345618', NULL, '决策委员会成员_测试账号', '1', NULL, NULL, NULL);
INSERT INTO `t_system_code_t` VALUES (43, 'test_account', '13512345619', NULL, '资方_测试账号', '1', NULL, NULL, NULL);
INSERT INTO `t_system_code_t` VALUES (44, 'test_account', '16612345602', NULL, '测试账号-2', '1', NULL, NULL, NULL);
INSERT INTO `t_system_code_t` VALUES (45, 'test_account', '16612345603', NULL, '测试账号-3', '1', NULL, NULL, NULL);
INSERT INTO `t_system_code_t` VALUES (46, 'test_account', '16612345604', NULL, '测试账号-4', '1', NULL, NULL, NULL);
INSERT INTO `t_system_code_t` VALUES (47, 'test_account', '16612345605', NULL, '测试账号-5', '1', NULL, NULL, NULL);
INSERT INTO `t_system_code_t` VALUES (48, 'test_account', '16612345606', NULL, '测试账号-6', '1', NULL, NULL, NULL);
INSERT INTO `t_system_code_t` VALUES (49, 'test_account', '16612345607', NULL, '测试账号-7', '1', NULL, NULL, NULL);
INSERT INTO `t_system_code_t` VALUES (50, 'test_account', '16612345608', NULL, '测试账号-8', '1', NULL, NULL, NULL);
INSERT INTO `t_system_code_t` VALUES (51, 'test_account', '16612345609', NULL, '测试账号-9', '1', NULL, NULL, NULL);
INSERT INTO `t_system_code_t` VALUES (52, 'test_account', '13512345601', NULL, '超级管理员_测试账号', '1', NULL, NULL, NULL);
INSERT INTO `t_system_code_t` VALUES (53, 'test_account', '13512345602', NULL, '委托方_测试账号', '1', NULL, NULL, NULL);
INSERT INTO `t_system_code_t` VALUES (54, 'test_account', '13512345603', NULL, '核心企业_测试账号', '1', NULL, NULL, NULL);
INSERT INTO `t_system_code_t` VALUES (57, 'sub_system', 'bee_supply_chain_finance', NULL, '供应链金融', '1', NULL, '2019-05-27 15:30:05', '2019-05-27 15:30:06');
INSERT INTO `t_system_code_t` VALUES (58, 'sub_system', 'bee_platform', NULL, '工业云平台', '1', NULL, '2019-05-27 15:30:08', '2019-05-27 15:30:09');
INSERT INTO `t_system_code_t` VALUES (59, 'sub_system', 'bee_iot', NULL, '蜂创物联', '1', NULL, '2019-05-27 15:30:12', '2019-05-27 15:30:13');
INSERT INTO `t_system_code_t` VALUES (60, 'sub_system', 'bee_logistics', NULL, '集蜂联运', '1', NULL, '2019-05-27 15:30:15', '2019-05-27 15:30:16');
INSERT INTO `t_system_code_t` VALUES (61, 'sub_system', 'bee_trade', NULL, '线上蜂贸', '1', NULL, '2019-05-27 15:30:20', '2019-05-27 15:30:21');
INSERT INTO `t_system_code_t` VALUES (62, 'sub_system', 'bee_industrial_brain_si', NULL, '工业大脑-硅系', '1', NULL, '2019-05-27 15:30:20', '2019-08-02 07:01:37');
INSERT INTO `t_system_code_t` VALUES (63, 'role_type', 'application', '', '应用类角色', '1', NULL, NULL, '2019-05-21 14:08:23');
INSERT INTO `t_system_code_t` VALUES (64, 'role_type', 'function_one', NULL, '功能类一级角色', '1', NULL, NULL, '2019-05-24 10:39:49');
INSERT INTO `t_system_code_t` VALUES (65, 'role_type', 'function_two', '', '功能类二级角色', '1', NULL, NULL, '2019-05-24 10:40:18');
INSERT INTO `t_system_code_t` VALUES (66, 'role_type', 'user_role', NULL, '用户自定义角色', '1', NULL, NULL, '2019-06-04 17:11:47');
INSERT INTO `t_system_code_t` VALUES (67, 'role_type', 'base', '', '基础类角色', '1', NULL, NULL, '2019-05-24 10:40:10');
INSERT INTO `t_system_code_t` VALUES (68, 'role_type', 'enterprise_admin', NULL, '企业管理员', '1', NULL, NULL, '2019-05-24 10:40:08');
INSERT INTO `t_system_code_t` VALUES (69, 'role_type', 'super_admin', NULL, '超级管理员', '1', NULL, NULL, '2019-05-24 10:40:03');
INSERT INTO `t_system_code_t` VALUES (70, 'sub_system', 'bee_industrial_brain', NULL, '工业大脑', '1', NULL, '2019-05-27 15:30:30', '2019-05-27 15:30:32');
INSERT INTO `t_system_code_t` VALUES (71, 'interface_type', 'PUT', NULL, '权限系统', '1', NULL, '2019-05-29 11:20:19', '2019-05-29 11:20:21');
INSERT INTO `t_system_code_t` VALUES (73, 'interface_type', 'GET', NULL, '权限系统', '1', NULL, '2019-05-29 11:20:22', '2019-05-29 11:20:28');
INSERT INTO `t_system_code_t` VALUES (74, 'interface_type', 'POST', NULL, '权限系统', '1', NULL, '2019-05-29 11:21:13', '2019-05-29 11:21:16');
INSERT INTO `t_system_code_t` VALUES (75, 'interface_type', 'DELETE', NULL, '权限系统', '1', NULL, '2019-05-29 11:21:42', '2019-05-29 11:21:45');
INSERT INTO `t_system_code_t` VALUES (80, 'sub_system', 'bee_console', NULL, '工业云平台-后台', '1', NULL, '2019-08-09 10:56:18', '2019-08-09 10:56:18');
INSERT INTO `t_system_code_t` VALUES (86, 'sub_system', 'bee_oa', NULL, 'EKP系统', '1', NULL, '2019-06-12 17:10:14', '2019-06-12 17:10:14');
INSERT INTO `t_system_code_t` VALUES (87, 'sub_system', 'wly_supply_chain_finance', NULL, '供应链金融体验', '1', NULL, '2019-07-12 15:53:56', '2019-07-12 15:53:56');

SET FOREIGN_KEY_CHECKS = 1;
