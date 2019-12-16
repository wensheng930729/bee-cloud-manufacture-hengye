package com.bee.platform.cloud.user.entity;

import java.io.Serializable;

import com.baomidou.mybatisplus.enums.IdType;
import java.util.Date;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.enums.IdType;
import com.baomidou.mybatisplus.activerecord.Model;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * <p>
 * 企业表
 * </p>
 *
 * @author zhigang.zhou123
 * @since 2019-09-19
 */

@Data
@NoArgsConstructor
@EqualsAndHashCode(callSuper = true)
@Accessors(chain = true)
public class AuthEnterprise extends Model<AuthEnterprise> {

    private static final long serialVersionUID = 1L;

    /**
     * 主键id
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 公司编号
     */
    private String enterpriseNo;
    /**
     * 公司全称
     */
    private String name;
    /**
     * 公司简称
     */
    private String simpleName;
    /**
     * 企业管理员
     */
    private String admin;
    /**
     * 上级公司
     */
    private Integer pid;
    /**
     * 企业类型 1企业 2物流商(兼容旧版本)
     */
    private Integer type;
    /**
     * 公司联系方式
     */
    private String contact;
    /**
     * 指定联系人
     */
    private String linkman;
    /**
     * 详细街道地址（(兼容旧版本)）
     */
    private String street;
    /**
     * 县级地区id
     */
    private Integer regionid;
    /**
     * 地址
     */
    private String address;
    /**
     * 所属行业
     */
    private Integer industry;
    /**
     * 状态：1启动 0禁用
     */
    private Integer status;
    /**
     * 是否删除 0未删除 1删除
     */
    private Integer deleted;
    /**
     * 可以添加子公司数目
     */
    private Integer childNum;
    /**
     * 删除原因
     */
    private String reason;
    /**
     * 操作者id（记录企业的添加者）
     */
    private Integer operateId;
    /**
     * 创建时间
     */
    private Date createTime;
    /**
     * 更新时间
     */
    private Date updateTime;
    /**
     * 删除时间
     */
    private Date deletedTime;



    @Override
    protected Serializable pkVal() {
        return this.id;
    }


}
