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
 * 配料明细统计表
 * </p>
 *
 * @author MP123
 * @since 2019-10-12
 */
@Data
@EqualsAndHashCode(callSuper = true)
@Accessors(chain = true)
public class ProIngredientStatistic extends Model<ProIngredientStatistic> {

    private static final long serialVersionUID = 1L;

    @TableId(value = "id", type = IdType.AUTO)
    private Long id;
    /**
     * 成品id
     */
    private Integer finishProductId;
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
     * 料批id
     */
    private Long batchId;
    /**
     * 料批名称
     */
    private String materialName;
    /**
     * plc设备id
     */
    private Integer plcId;
    /**
     * plc设备名称
     */
    private String plcName;
    /**
     * 产品单位
     */
    private String unit;
    /**
     * 产品数量
     */
    private BigDecimal num;
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
     * 数据状态0删除1正常
     */
    private Integer status;


    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
