package com.bee.platform.cloud.si.manufacture.entity;

import java.io.Serializable;

import java.util.Date;

import cn.afterturn.easypoi.excel.annotation.Excel;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.enums.IdType;
import com.baomidou.mybatisplus.activerecord.Model;
import java.io.Serializable;

import com.baomidou.mybatisplus.annotations.Version;

import com.fasterxml.jackson.annotation.JsonFormat;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.Accessors;

/**
 * <p>
 * 产品规格表
 * </p>
 *
 * @author MP123
 * @since 2019-10-15
 */
@Data
@EqualsAndHashCode(callSuper = true)
@Accessors(chain = true)
public class ConfigProductSpec extends Model<ConfigProductSpec> {

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
     * 企业id
     */
    @Excel(name = "企业id", orderNum = "2", type = 10, width = 20)
    private Integer enterpriseId;
    /**
     * 工厂id
     */
    @Excel(name = "工厂id", orderNum = "3", type = 10, width = 20)
    private Integer factoryId;
    /**
     * 产品id
     */
    @Excel(name = "产品id", orderNum = "4", type = 10, width = 20)
    private Integer productId;

    /**
     * 产品名称
     */
    @Excel(name = "产品名称", orderNum = "5", width = 20)
    private String productName;
    /**
     * 规格名称
     */
    @Excel(name = "规格名称" ,orderNum = "6",width = 20)
    private String specName;
    /**
     * 合格线（0 否 1 是）
     */
    @Excel(name = "合格线（0 否 1 是）", orderNum = "7", type = 10, width = 20)
    private Integer qualifiedLine;
    /**
     * 状态(1-启用 ,0-禁用)
     */
    @Excel(name = "状态(1-启用 ,0-禁用)", orderNum = "8", type = 10, width = 20)
    private Integer status;
    /**
     * 排序
     */
    @Excel(name = "排序", orderNum = "9", type = 10, width = 20)
    private Integer sort;
    /**
     * 是否删除 0未删除 1删除
     */
    @Excel(name = "是否删除 0未删除 1删除", orderNum = "10", type = 10, width = 20)
    private Integer deleted;
    /**
     * 创建人id
     */
    @Excel(name = "创建人id", orderNum = "11", type = 10, width = 20)
    private Integer createId;
    /**
     * 创建人名称
     */
    @Excel(name = "创建人名称" ,orderNum = "12",width = 20)
    private String creator;
    /**
     * 创建时间
     */
    @Excel(name = "创建时间",orderNum = "13",width = 20, format = "yyyy-MM-dd HH:mm:ss")
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    private Date createTime;
    /**
     * 修改人id
     */
    @Excel(name = "修改人id", orderNum = "14", type = 10, width = 20)
    private Integer modifyId;
    /**
     * 修改人名称
     */
    @Excel(name = "修改人名称" ,orderNum = "15",width = 20)
    private String modifier;
    /**
     * 修改时间
     */
    @Excel(name = "修改时间",orderNum = "16",width = 20, format = "yyyy-MM-dd HH:mm:ss")
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    private Date modifyTime;


    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
