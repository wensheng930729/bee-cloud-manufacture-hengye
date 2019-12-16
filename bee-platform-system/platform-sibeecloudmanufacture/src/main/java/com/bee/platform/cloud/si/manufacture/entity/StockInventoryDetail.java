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
 * 库存盘点详细
 * </p>
 *
 * @author junyang.li123
 * @since 2019-11-25
 */
@Data
@EqualsAndHashCode(callSuper = true)
@Accessors(chain = true)
public class StockInventoryDetail extends Model<StockInventoryDetail> {

    private static final long serialVersionUID = 1L;

    /**
     * 主键
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 盘点主表盘点单id
     */
    private String inventoryOrderId;
    /**
     * 产品id
     */
    private Integer productId;
    /**
     * 仓库id
     */
    private Integer storageId;
    /**
     * 产品规格id
     */
    private Integer productSpecId;
    /**
     * 产品计量单位
     */
    private String productUnit;
    /**
     * 账面数量
     */
    private BigDecimal accountNum;
    /**
     * 实盘数量
     */
    private BigDecimal actualNum;
    /**
     * 差异数量
     */
    private BigDecimal differenceNum;
    /**
     * 是否有效0无效，1有效
     */
    private Integer status;
    /**
     * 创建人id
     */
    private Integer createId;
    /**
     * 创建人姓名
     */
    private String creator;
    /**
     * 创建时间
     */
    private Date createTime;
    /**
     * 修改人id
     */
    private Integer modifyId;
    /**
     * 修改人姓名
     */
    private String modifier;
    /**
     * 修改时间
     */
    private Date modifyTime;


    @Override
    protected Serializable pkVal() {
        return this.id;
    }

    public StockInventoryDetail() {
    }

    public StockInventoryDetail(String inventoryOrderId, Integer productId, Integer storageId, Integer productSpecId,
                                String productUnit, BigDecimal accountNum, BigDecimal actualNum, Integer status,
                                Integer createId, String creator, Date createTime) {
        this.inventoryOrderId = inventoryOrderId;
        this.productId = productId;
        this.storageId = storageId;
        this.productSpecId = productSpecId;
        this.productUnit = productUnit;
        this.accountNum = accountNum;
        this.actualNum = actualNum;
        this.status = status;
        this.createId = createId;
        this.creator = creator;
        this.createTime = createTime;
    }
}
