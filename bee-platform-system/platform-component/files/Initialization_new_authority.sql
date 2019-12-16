-- auth_contact
CREATE TABLE `auth_contact` (
  `id` int(20) NOT NULL AUTO_INCREMENT COMMENT '联系人主键id',
  `contact_no` varchar(200) DEFAULT NULL COMMENT '联系人编号',
  `second_type` varchar(200) DEFAULT NULL COMMENT '二级分类',
  `name` varchar(20) DEFAULT NULL COMMENT '联系人姓名',
  `sex` varchar(20) DEFAULT NULL COMMENT '性别',
  `phone` varchar(20) DEFAULT NULL COMMENT '电话号码',
  `fixtel` varchar(20) DEFAULT NULL COMMENT '座机',
  `birthday` varchar(20) DEFAULT NULL COMMENT '生日',
  `address` varchar(200) DEFAULT NULL COMMENT '联系人的邮寄地址',
  `hobby` varchar(20) DEFAULT NULL COMMENT '爱好',
  `star_level` int(20) DEFAULT NULL COMMENT '联系人星级评定：1是一星 2是二星 3是三星 4是四星 5是五星',
  `work_brief` varchar(200) DEFAULT NULL COMMENT '工作简介',
  `status` int(10) DEFAULT NULL COMMENT '联系人状态： 1是启用 2是禁用',
  `create_time` datetime DEFAULT NULL COMMENT '创建时间',
  `update_time` datetime DEFAULT NULL COMMENT '更新时间',
  `deleted` tinyint(10) DEFAULT NULL COMMENT '表示逻辑删除，1-是删除，0-不删除',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- auth_customer
CREATE TABLE `auth_customer` (
  `id` int(20) NOT NULL AUTO_INCREMENT COMMENT '客户表主键ID',
  `enterprise_id` int(20) NOT NULL COMMENT '企业ID',
  `cus_no` varchar(50) DEFAULT NULL COMMENT '客户编码',
  `cus_name` varchar(30) DEFAULT NULL COMMENT '客户姓名',
  `cus_first_type` varchar(200) DEFAULT NULL COMMENT '客户一级分类',
  `cus_second_type` varchar(200) DEFAULT NULL COMMENT '客户二级分类',
  `status` tinyint(10) DEFAULT NULL COMMENT '用户状态：1启用 2禁用',
  `deleted` tinyint(10) DEFAULT NULL COMMENT '表示逻辑删除，1-是删除，0-不删除',
  `create_time` datetime DEFAULT NULL COMMENT '创建时间',
  `update_time` datetime DEFAULT NULL COMMENT '更新时间',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- auth_customer_contact
CREATE TABLE `auth_customer_contact` (
  `id` int(20) NOT NULL AUTO_INCREMENT COMMENT '主键',
  `customer_id` int(20) NOT NULL COMMENT '客户ID',
  `contact_id` int(20) NOT NULL COMMENT '联系人ID',
  `status` tinyint(10) DEFAULT NULL COMMENT '状态 ：1是启用 2是禁用',
  `order_num` int(11) DEFAULT NULL COMMENT '该客户下角色的序列号',
  `create_user` varchar(20) DEFAULT NULL COMMENT '创建人',
  `create_time` datetime DEFAULT NULL COMMENT '创建时间',
  `update_time` datetime DEFAULT NULL COMMENT '更新时间',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;


-- 客户关联角色的中间表
CREATE TABLE `auth_customer_role` (
  `id` int(11) NOT NULL AUTO_INCREMENT COMMENT '主键id',
  `customer_id` int(11) NOT NULL COMMENT '功能id',
  `role_id` int(11) NOT NULL COMMENT '角色id',
  `status` int(2) DEFAULT NULL COMMENT '是否启用：1启用，0禁用',
  `order_num` int(11) DEFAULT NULL COMMENT '该角色下角色的序号',
  `create_user` int(11) DEFAULT NULL COMMENT '创建人',
  `create_time` datetime DEFAULT NULL COMMENT '创建时间',
  `update_time` datetime DEFAULT NULL COMMENT '更新时间',
  `deleted` tinyint(2) DEFAULT NULL COMMENT '逻辑删除，1-是，0-否',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='客户关联角色的中间表';

-- 企业表
CREATE TABLE `auth_enterprise` (
  `id` int(11) NOT NULL AUTO_INCREMENT COMMENT '主键id',
  `enterprise_no` varchar(50) NOT NULL COMMENT '公司编号',
  `name` varchar(60) NOT NULL COMMENT '公司全称',
  `simple_name` varchar(60) DEFAULT NULL COMMENT '公司简称',
  `logo` varchar(100) DEFAULT NULL COMMENT '公司logo',
  `pid` int(11) DEFAULT NULL COMMENT '上级公司',
  `status` tinyint(2) NOT NULL COMMENT '状态：1启动 0禁用',
  `deleted` tinyint(2) NOT NULL COMMENT '是否删除 0未删除 1删除',
  `create_time` datetime NOT NULL COMMENT '创建时间',
  `update_time` datetime DEFAULT NULL COMMENT '更新时间',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='企业表';

-- 企业与角色（角色或功能的关联表）的中间表
CREATE TABLE `auth_enterprise_role` (
  `id` int(11) NOT NULL AUTO_INCREMENT COMMENT '主键id',
  `enterprise_id` int(11) NOT NULL COMMENT '企业id',
  `role_id` int(11) NOT NULL COMMENT '角色id',
  `level` int(11) NOT NULL COMMENT '该用户下角色级别：与角色表的level对应',
  `type` varchar(20) DEFAULT NULL COMMENT '该用户下角色或功能的自定义分类。基于level字段的一个分类，用于分类展示',
  `order_num` int(11) DEFAULT NULL COMMENT '该用户下角色的序号',
  `status` tinyint(2) NOT NULL COMMENT '状态：1启动 0禁用',
  `deleted` tinyint(2) NOT NULL COMMENT '是否删除 0未删除 1删除',
  `create_user` int(11) NOT NULL COMMENT '创建人',
  `create_time` datetime NOT NULL COMMENT '创建时间',
  `update_time` datetime DEFAULT NULL COMMENT '更新时间',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='企业与角色（角色或功能的关联表）的中间表';


-- 功能关联角色的中间表
CREATE TABLE `auth_function_role` (
  `id` int(11) NOT NULL AUTO_INCREMENT COMMENT '主键id',
  `function_id` int(11) NOT NULL COMMENT '功能id',
  `role_id` int(11) NOT NULL COMMENT '角色id',
  `status` int(2) DEFAULT NULL COMMENT '是否启用：1启用，0禁用',
  `order_num` int(11) DEFAULT NULL COMMENT '该功能下角色的序号',
  `create_user` int(11) DEFAULT NULL COMMENT '创建人id',
  `create_time` datetime DEFAULT NULL COMMENT '创建时间',
  `update_time` datetime DEFAULT NULL COMMENT '更新时间',
  `deleted` tinyint(2) unsigned DEFAULT NULL COMMENT '逻辑删除，1-是，0-否',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='功能关联角色的中间表';

-- auth_interface
CREATE TABLE `auth_interface` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT COMMENT '资源id',
  `name` varchar(70) NOT NULL COMMENT '资源名称',
  `type` int(10) NOT NULL COMMENT '资源类型',
  `sub_sys` varchar(20) DEFAULT NULL COMMENT '子系统标识',
  `order_num` int(11) DEFAULT NULL COMMENT '排序',
  `url` varchar(200) NOT NULL COMMENT '资源地址',
  `status` int(2) NOT NULL DEFAULT '0' COMMENT '是否启用：1启用 0禁用 ',
  `create_time` datetime NOT NULL COMMENT '创建时间',
  `update_time` datetime DEFAULT NULL COMMENT '更新时间',
  `deleted` tinyint(2) NOT NULL DEFAULT '0' COMMENT '是否删除：1是 0否',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- auth_platform_user
CREATE TABLE `auth_platform_user` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT COMMENT '用户id',
  `enterprise_id` int(11) unsigned DEFAULT NULL COMMENT '归属公司id',
  `phone` varchar(20) NOT NULL COMMENT '手机号',
  `name` varchar(60) NOT NULL COMMENT '姓名',
  `sex` varchar(2) NOT NULL COMMENT '性别：M男 F女',
  `username` varchar(60) NOT NULL COMMENT '用户账号：现在是手机号',
  `nickname` varchar(60) NOT NULL COMMENT '用户名',
  `password` varchar(200) DEFAULT NULL COMMENT '密码',
  `head` varchar(255) DEFAULT NULL COMMENT '头像',
  `status` int(2) NOT NULL DEFAULT '0' COMMENT '是否启用：1启用 0禁用 ',
  `create_time` datetime NOT NULL COMMENT '创建时间',
  `update_time` datetime DEFAULT NULL COMMENT '更新时间',
  `deleted` tinyint(2) NOT NULL DEFAULT '0' COMMENT '是否删除：1是 0否',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- 企业与用户中间表
CREATE TABLE `auth_platform_user_enterprise` (
  `id` int(11) NOT NULL AUTO_INCREMENT COMMENT '主键id',
  `user_id` int(11) NOT NULL COMMENT '用户id',
  `enterprise_id` int(11) NOT NULL COMMENT '企业id',
  `status` tinyint(2) NOT NULL COMMENT '状态：1启动 0禁用',
  `deleted` tinyint(2) NOT NULL COMMENT '是否删除 0未删除 1删除',
  `create_user` int(11) DEFAULT NULL COMMENT '创建人',
  `create_time` datetime NOT NULL COMMENT '创建时间',
  `update_time` datetime DEFAULT NULL COMMENT '更新时间',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='企业与用户中间表';

-- 资源表
CREATE TABLE `auth_resource` (
  `id` int(10) NOT NULL AUTO_INCREMENT COMMENT '菜单id',
  `pid` int(10) DEFAULT NULL COMMENT '父id',
  `sub_sys` varchar(20) NOT NULL COMMENT '子系统标识',
  `text` varchar(70) NOT NULL COMMENT '菜单名称',
  `type` tinyint(1) NOT NULL COMMENT '菜单类型',
  `icon` varchar(200) DEFAULT NULL COMMENT '菜单图标',
  `url` varchar(200) DEFAULT NULL COMMENT '菜单url',
  `order_num` int(11) NOT NULL COMMENT '菜单序号',
  `is_hide` tinyint(1) DEFAULT NULL COMMENT '是否隐藏0展开1隐藏',
  `deleted` tinyint(1) NOT NULL COMMENT '是否删除0未删除1已删除',
  `create_time` datetime NOT NULL COMMENT '创建时间',
  `update_time` datetime DEFAULT NULL COMMENT '修改时间',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE=InnoDB AUTO_INCREMENT=90 DEFAULT CHARSET=utf8 COMMENT='资源表';

-- 角色表
CREATE TABLE `auth_role` (
  `id` int(10) NOT NULL AUTO_INCREMENT COMMENT '角色id',
  `role_name` varchar(32) NOT NULL COMMENT '角色名称',
  `role_type` tinyint(1) NOT NULL COMMENT '角色类型',
  `level` tinyint(2) NOT NULL COMMENT '角色级别',
  `sub_sys` varchar(20) DEFAULT NULL COMMENT '子系统标识',
  `deleted` tinyint(1) NOT NULL COMMENT '是否删除0未删除1已删除',
  `create_user` int(11) NOT NULL COMMENT '创建人',
  `create_time` datetime NOT NULL COMMENT '创建时间',
  `update_time` datetime DEFAULT NULL COMMENT '修改时间',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE=InnoDB AUTO_INCREMENT=90 DEFAULT CHARSET=utf8 COMMENT='角色表';

-- auth_role_interface
CREATE TABLE `auth_role_interface` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT COMMENT '主键id',
  `role_id` int(11) unsigned NOT NULL COMMENT '角色id',
  `interface_id` int(11) unsigned NOT NULL COMMENT '接口id',
  `status` int(2) NOT NULL DEFAULT '0' COMMENT '是否启用：1启用 0禁用 ',
  `order_num_id` int(11) unsigned DEFAULT NULL COMMENT '该角色下接口序号',
  `create_user` int(11) unsigned NOT NULL COMMENT '创建人',
  `create_time` datetime NOT NULL COMMENT '创建时间',
  `update_time` datetime DEFAULT NULL COMMENT '更新时间',
  `deleted` tinyint(2) NOT NULL DEFAULT '0' COMMENT '是否删除：1是 0否',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- 资源角色表
CREATE TABLE `auth_role_resource` (
  `id` int(10) NOT NULL AUTO_INCREMENT COMMENT '主键id',
  `role_id` int(10) NOT NULL COMMENT '角色id',
  `resource_id` int(10) NOT NULL COMMENT '菜单id',
  `order_num` int(11) NOT NULL COMMENT '菜单序号',
  `deleted` tinyint(1) NOT NULL COMMENT '是否删除0未删除1已删除',
  `create_user` int(11) NOT NULL COMMENT '创建人',
  `create_time` datetime NOT NULL COMMENT '创建时间',
  `update_time` datetime DEFAULT NULL COMMENT '修改时间',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE=InnoDB AUTO_INCREMENT=90 DEFAULT CHARSET=utf8 COMMENT='资源角色表';

-- 用户与角色（角色或功能的关联表）的中间表
CREATE TABLE `auth_user_role` (
  `id` int(11) NOT NULL AUTO_INCREMENT COMMENT '主键id',
  `user_id` int(11) NOT NULL COMMENT '用户id',
  `role_id` int(11) NOT NULL COMMENT '角色id',
  `status` int(2) DEFAULT NULL COMMENT '是否启用：1启用，0禁用',
  `level` int(11) DEFAULT NULL COMMENT '该用户下角色级别：与角色表的level对应',
  `type` varchar(20) DEFAULT NULL COMMENT '该用户下角色或功能的自定义分类，基于level字段的一个分类，用于分类展示',
  `order_num` int(11) DEFAULT NULL COMMENT '该用户下角色的序号',
  `create_user` int(11) DEFAULT NULL COMMENT '创建人id',
  `create_time` datetime DEFAULT NULL COMMENT '创建时间',
  `update_time` datetime DEFAULT NULL COMMENT '更新时间',
  `deleted` tinyint(2) unsigned DEFAULT NULL COMMENT '逻辑删除，1-是，0-否',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='用户与角色（角色或功能的关联表）的中间表';