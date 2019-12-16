package com.bee.platform.cloud.si.manufacture.entity;

import cn.afterturn.easypoi.excel.annotation.Excel;
import com.baomidou.mybatisplus.activerecord.Model;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.enums.IdType;
import com.fasterxml.jackson.annotation.JsonFormat;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

/**
 * <p>
 * 产品档案
 * </p>
 *
 * @author chenxm66777123
 * @since 2019-09-23
 */
@Data
@NoArgsConstructor
@EqualsAndHashCode(callSuper = true)
@Accessors(chain = true)
public class ConfigProduct extends Model<ConfigProduct> {

    private static final long serialVersionUID = 1L;

    /**
     * 导出类型 1 是文本 2 是图片,3 是函数,10 是数字 默认是文本
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
     * 产品名称
     */
    @Excel(name = "产品名称" ,orderNum = "4",width = 20)
    private String name;
    /**
     * 产品logo
     */
    @Excel(name = "产品logo" ,orderNum = "5",width = 20)
    private String logo;
    /**
     * 单位code
     */
    @Excel(name = "单位code" ,orderNum = "6",width = 20)
    private String unitCode;
    /**
     * 单位value
     */
    @Excel(name = "单位value" ,orderNum = "7",width = 20)
    private String unitValue;
    /**
     * 产品类别id
     */
    @Excel(name = "产品类别id", orderNum = "8", type = 10, width = 20)
    private Integer categoryId;

    /**
     * 产品类别
     */
    @Excel(name = "产品类别" ,orderNum = "9",width = 20)
    private String categoryName;
    /**
     * 状态:1-启用,0-禁用
     */
    @Excel(name = "状态:1-启用,0-禁用", orderNum = "10", type = 10, width = 20)
    private Integer status;
    /**
     * 是否删除 0未删除 1删除
     */
    @Excel(name = "是否删除 0未删除 1删除", orderNum = "11", type = 10, width = 20)
    private Integer deleted;

    /**
     * 是否是标准品（0 否 1 是）
     */
    @Excel(name = "是否是标准品（0 否 1 是）", orderNum = "12", type = 10, width = 20)
    private Integer standard;
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
     * 修改人
     */
    @Excel(name = "修改人" ,orderNum = "17",width = 20)
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
