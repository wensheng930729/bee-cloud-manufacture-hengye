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
 * 仓库档案
 * </p>
 *
 * @author chenxm66777123
 * @since 2019-09-23
 */
@Data
@NoArgsConstructor
@EqualsAndHashCode(callSuper = true)
@Accessors(chain = true)
public class ConfigRepository extends Model<ConfigRepository> {

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
     * 仓库名称
     */
    @Excel(name = "仓库名称" ,orderNum = "4",width = 20)
    private String name;
    /**
     * 状态1:启用0:停用
     */
    @Excel(name = "状态1:启用0:停用", orderNum = "5", type = 10, width = 20)
    private Integer status;
    /**
     * 仓库类别(0 成品 1 原料 2 配料  3五金 4其他)
     */
    @Excel(name = "仓库类别(0 成品 1 原料 2 配料  3五金 4其他)", orderNum = "6", type = 10, width = 20)
    private Integer type;
    /**
     * 是否删除 1：表示删除 0：表示未删除
     */
    @Excel(name = "是否删除 1：表示删除 0：表示未删除", orderNum = "7", type = 10, width = 20)
    private Integer deleted;
    /**
     * 创建人id
     */
    @Excel(name = "创建人id", orderNum = "8", type = 10, width = 20)
    private Integer createId;
    /**
     * 创建人名称
     */
    @Excel(name = "创建人名称" ,orderNum = "9",width = 20)
    private String creator;
    /**
     * 创建时间
     */
    @Excel(name = "创建时间",orderNum = "10",width = 20, format = "yyyy-MM-dd HH:mm:ss")
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    private Date createTime;
    /**
     * 修改人id
     */
    @Excel(name = "修改人id", orderNum = "11", type = 10, width = 20)
    private Integer modifyId;
    /**
     * 修改人
     */
    @Excel(name = "修改人" ,orderNum = "12",width = 20)
    private String modifier;
    /**
     * 修改时间
     */
    @Excel(name = "修改时间",orderNum = "13",width = 20, format = "yyyy-MM-dd HH:mm:ss")
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    private Date modifyTime;


    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
