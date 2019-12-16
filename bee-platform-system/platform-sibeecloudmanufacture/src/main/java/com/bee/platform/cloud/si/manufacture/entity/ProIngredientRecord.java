package com.bee.platform.cloud.si.manufacture.entity;

import com.baomidou.mybatisplus.activerecord.Model;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.enums.IdType;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;

/**
 * <p>
 * 配料明细统计表
 * </p>
 *
 * @author MP123
 * @since 2019-10-30
 */
@Data
@EqualsAndHashCode(callSuper = true)
@Accessors(chain = true)
public class ProIngredientRecord extends Model<ProIngredientRecord> {

    private static final long serialVersionUID = 1L;

    @TableId(value = "id", type = IdType.AUTO)
    private Long id;
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
     * 工厂id
     */
    private Integer factoryId;
    /**
     * 企业id
     */
    private Integer companyId;
    /**
     * 数据状态0删除1正常
     */
    private Integer status;


    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
