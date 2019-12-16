package com.bee.platform.cloud.user.entity;

import java.io.Serializable;

import cn.afterturn.easypoi.excel.annotation.Excel;
import com.baomidou.mybatisplus.enums.IdType;
import java.util.Date;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.enums.IdType;
import com.baomidou.mybatisplus.activerecord.Model;
import java.io.Serializable;

import com.baomidou.mybatisplus.annotations.Version;

import com.fasterxml.jackson.annotation.JsonFormat;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

/**
 * <p>
 * 客户账号和供应商账号
 * </p>
 *
 * @author junyang.li123
 * @since 2019-10-18
 */
@Data
@NoArgsConstructor
@EqualsAndHashCode(callSuper = true)
@Accessors(chain = true)
public class AuthCustomerOrSupplierAccount extends Model<AuthCustomerOrSupplierAccount> {

    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    @TableId(value = "id", type = IdType.AUTO)
    @Excel(name = "id", orderNum = "1", type = 10, width = 20)
    private Integer id;
    /**
     * 所属企业id
     */
    @Excel(name = "所属企业id", orderNum = "2", type = 10, width = 20)
    private Integer enterpriseId;
    /**
     * 工厂id
     */
    @Excel(name = "工厂id", orderNum = "3", type = 10, width = 20)
    private Integer factoryId;
    /**
     * 关联id(客户id 或者 供应商id）
     */
    @Excel(name = "关联id(客户id 或者 供应商id）", orderNum = "4", type = 10, width = 20)
    private Integer relatedId;
    /**
     * 姓名
     */
    @Excel(name = "姓名" ,orderNum = "5",width = 20)
    private String name;
    /**
     * 注册手机号
     */
    @Excel(name = "注册手机号" ,orderNum = "6",width = 20)
    private String phone;
    /**
     * 邮箱
     */
    @Excel(name = "邮箱" ,orderNum = "7",width = 20)
    private String mailbox;
    /**
     * 启用状态（ 0禁用 1启用 ）
     */
    @Excel(name = "启用状态（ 0禁用 1启用 ）", orderNum = "8", type = 10, width = 20)
    private Integer status;
    /**
     * 职务
     */
    @Excel(name = "职务" ,orderNum = "9",width = 20)
    private String job;
    /**
     * 是否是默认公司(0 不是 1 是)
     */
    @Excel(name = "是否是默认公司(0 不是 1 是)", orderNum = "10", type = 10, width = 20)
    private Integer defaultEnterprise;
    /**
     * 分类（0 客户 1供应商）
     */
    @Excel(name = "分类（0 客户 1供应商）", orderNum = "11", type = 10, width = 20)
    private Integer type;
    /**
     * 是否删除 0未删除 1删除
     */
    @Excel(name = "是否删除 0未删除 1删除", orderNum = "12", type = 10, width = 20)
    private Integer deleted;
    /**
     * 创建人id
     */
    @Excel(name = "创建人id", orderNum = "13", type = 10, width = 20)
    private Integer createId;
    /**
     * 创建人名称
     */
    @Excel(name = "创建人名称" ,orderNum = "14",width = 20)
    private String creator;
    /**
     * 创建时间
     */
    @Excel(name = "创建时间",orderNum = "15",width = 20, format = "yyyy-MM-dd HH:mm:ss")
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    private Date createTime;
    /**
     * 修改人id
     */
    @Excel(name = "修改人id", orderNum = "16", type = 10, width = 20)
    private Integer modifyId;
    /**
     * 修改人名称
     */
    @Excel(name = "修改人名称" ,orderNum = "17",width = 20)
    private String modifier;
    /**
     * 修改时间
     */
    @Excel(name = "修改时间",orderNum = "18",width = 20, format = "yyyy-MM-dd HH:mm:ss")
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    private Date modifyTime;


    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
