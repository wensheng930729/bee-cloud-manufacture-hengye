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
 * 产品结算属性
 * </p>
 *
 * @author chenxm66777123
 * @since 2019-09-23
 */
@Data
@NoArgsConstructor
@EqualsAndHashCode(callSuper = true)
@Accessors(chain = true)
public class ConfigProductSettlementAttribute extends Model<ConfigProductSettlementAttribute> {

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
     * 产品id
     */
    @Excel(name = "产品id", orderNum = "4", type = 10, width = 20)
    private Integer productId;
    /**
     * 结算项
     */
    @Excel(name = "结算项" ,orderNum = "5",width = 20)
    private String settlementItem;
    /**
     * 字段限制（0 无限制 1 整数 2 数值 小数点后两位 3 百分比）
     */
    @Excel(name = "字段限制（0 无限制 1 整数 2 数值 小数点后两位 3 百分比）", orderNum = "6", type = 10, width = 20)
    private Integer fieldLimit;
    /**
     * 单位
     */
    @Excel(name = "单位" ,orderNum = "7",width = 20)
    private String unit;
    /**
     * 状态 1启用 0未启用
     */
    @Excel(name = "状态 1启用 0未启用", orderNum = "8", type = 10, width = 20)
    private Integer status;
    /**
     * 是否删除 0未删除 1删除
     */
    @Excel(name = "是否删除 0未删除 1删除", orderNum = "9", type = 10, width = 20)
    private Integer deleted;
    /**
     * 创建人id
     */
    @Excel(name = "创建人id", orderNum = "10", type = 10, width = 20)
    private Integer createId;
    /**
     * 创建人名称
     */
    @Excel(name = "创建人名称" ,orderNum = "11",width = 20)
    private String creator;
    /**
     * 创建时间
     */
    @Excel(name = "创建时间",orderNum = "12",width = 20, format = "yyyy-MM-dd HH:mm:ss")
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    private Date createTime;
    /**
     * 修改人id
     */
    @Excel(name = "修改人id", orderNum = "13", type = 10, width = 20)
    private Integer modifyId;
    /**
     * 修改人
     */
    @Excel(name = "修改人" ,orderNum = "14",width = 20)
    private String modifier;
    /**
     * 修改时间
     */
    @Excel(name = "修改时间",orderNum = "15",width = 20, format = "yyyy-MM-dd HH:mm:ss")
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    private Date modifyTime;


    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
