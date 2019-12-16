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
 * 供应商或者客户管理
 * </p>
 *
 * @author junyang.li123
 * @since 2019-10-21
 */
@Data
@NoArgsConstructor
@EqualsAndHashCode(callSuper = true)
@Accessors(chain = true)
public class AuthCustomerOrSupplier extends Model<AuthCustomerOrSupplier> {

    private static final long serialVersionUID = 1L;


    /**
     * 导出类型 1 是文本 2 是图片,3 是函数,10 是数字 默认是文本
     */

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
     * 客户或供应商名称
     */
    @Excel(name = "客户或供应商名称" ,orderNum = "4",width = 20)
    private String name;
    /**
     * 联系方式
     */
    @Excel(name = "联系方式" ,orderNum = "5",width = 20)
    private String telephone;
    /**
     * 地址
     */
    @Excel(name = "地址" ,orderNum = "6",width = 20)
    private String address;
    /**
     * 公司邮箱
     */
    @Excel(name = "公司邮箱" ,orderNum = "7",width = 20)
    private String mailbox;
    /**
     * 客户或供应商类别（0核心客户或供应商 1战略客户或供应商  2储备客户或供应商）
     */
    @Excel(name = "客户或供应商类别（0核心客户或供应商 1战略客户或供应商  2储备客户或供应商）", orderNum = "8", type = 10, width = 20)
    private Integer category;
    /**
     * 启用状态（ 0禁用 1启用 ）
     */
    @Excel(name = "启用状态（ 0禁用 1启用 ）", orderNum = "9", type = 10, width = 20)
    private Integer status;
    /**
     * 分类（0 客户 1供应商）
     */
    @Excel(name = "分类（0 客户 1供应商）", orderNum = "10", type = 10, width = 20)
    private Integer type;
    /**
     * 相关联系人
     */
    @Excel(name = "相关联系人" ,orderNum = "11",width = 20)
    private String relatedContacts;
    /**
     * 电话号码
     */
    @Excel(name = "电话号码" ,orderNum = "12",width = 20)
    private String phoneNumber;
    /**
     * 是否为承运商（0否 1是）
     */
    @Excel(name = "是否为承运商（0否 1是）", orderNum = "13", type = 10, width = 20)
    private Integer carrier;

    /**
     * 承运商类别（0公司 1个人）
     */
    @Excel(name = "承运商类别(0公司 1个人)", orderNum = "14", type = 10, width = 20)
    private Integer carrierCategory;
    /**
     * 银行账号
     */
    @Excel(name = "银行账号" ,orderNum = "15",width = 20)
    private String bankAccount;
    /**
     * 开户银行
     */
    @Excel(name = "开户银行" ,orderNum = "16",width = 20)
    private String bank;
    /**
     * 公司税号
     */
    @Excel(name = "公司税号" ,orderNum = "17",width = 20)
    private String taxNumber;
    /**
     * 开票地址
     */
    @Excel(name = "开票地址" ,orderNum = "18",width = 20)
    private String billingAddress;
    /**
     * 是否删除 0未删除 1删除
     */
    @Excel(name = "是否删除 0未删除 1删除", orderNum = "19", type = 10, width = 20)
    private Integer deleted;
    /**
     * 创建人id
     */
    @Excel(name = "创建人id", orderNum = "20", type = 10, width = 20)
    private Integer createId;
    /**
     * 创建人名称
     */
    @Excel(name = "创建人名称" ,orderNum = "21",width = 20)
    private String creator;
    /**
     * 创建时间
     */
    @Excel(name = "创建时间",orderNum = "22",width = 20, format = "yyyy-MM-dd HH:mm:ss")
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    private Date createTime;
    /**
     * 修改人id
     */
    @Excel(name = "修改人id", orderNum = "23", type = 10, width = 20)
    private Integer modifyId;
    /**
     * 修改人名称
     */
    @Excel(name = "修改人名称" ,orderNum = "24",width = 20)
    private String modifier;
    /**
     * 修改时间
     */
    @Excel(name = "修改时间",orderNum = "25",width = 20, format = "yyyy-MM-dd HH:mm:ss")
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    private Date modifyTime;


    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
