package com.bee.platform.cloud.si.manufacture.entity;

import java.io.Serializable;

import com.baomidou.mybatisplus.enums.IdType;
import java.math.BigDecimal;
import java.util.Date;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.enums.IdType;
import com.baomidou.mybatisplus.activerecord.Model;
import java.io.Serializable;

import com.baomidou.mybatisplus.annotations.Version;

import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.Accessors;

/**
 * <p>
 * 料批明细表
 * </p>
 *
 * @author huangxin123
 * @since 2019-09-24
 */
@Data
@EqualsAndHashCode(callSuper = true)
@Accessors(chain = true)
public class ProMaterialBatchDetail extends Model<ProMaterialBatchDetail> {

    private static final long serialVersionUID = 1L;

    /**
     * 料批明细id
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Long id;
    /**
     * 料批主表id
     */
    private Long batchId;
    /**
     * 产品id
     */
    private Integer productId;
    /**
     * 产品名称
     */
    private String productName;
    /**
     * 产品规格id
     */
    private Integer productSpecId;
    /**
     * 产品规格名称
     */
    private String productSpecName;
    /**
     * 产品数量
     */
    private BigDecimal num;
    /**
     * 产品单位
     */
    private String unit;
    /**
     * 质量要求
     */
    private String qualityRequirements;
    /**
     * 仓库id
     */
    private Integer warehouseId;
    /**
     * 仓库名称
     */
    private String warehouseName;
    /**
     * 创建时间
     */
    private Date createTime;
    /**
     * 更新时间
     */
    private Date modifyTime;
    /**
     * 数据状态:0删除1正常
     */
    private Integer status;
    /**
     * plc出料斗英文名称
     */
    private String plcField;
    /**
     * plc出料斗中文名
     */
    private String plcFieldName;


    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
