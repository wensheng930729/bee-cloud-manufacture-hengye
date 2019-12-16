package com.bee.platform.cloud.si.manufacture.entity;

import com.baomidou.mybatisplus.activerecord.Model;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.enums.IdType;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

/**
 * <p>
 * 库存盘点主表
 * </p>
 *
 * @author junyang.li123
 * @since 2019-11-25
 */
@Data
@EqualsAndHashCode(callSuper = true)
@Accessors(chain = true)
public class StockInventory extends Model<StockInventory> {

    private static final long serialVersionUID = 1L;

    /**
     * 主键
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 盘点单单号
     */
    private String inventoryOrderId;
    /**
     * 盘点单名称
     */
    private String inventoryName;
    /**
     * 盘点分类 。1 全盘 ,2产品分类盘点 ，3 产品盘点 4.仓库盘点
     */
    private Integer inventoryType;
    /**
     * 工厂id
     */
    private Integer factoryId;
    /**
     * 企业id
     */
    private Integer enterpriseId;
    /**
     * 备注
     */
    private String remarks;
    /**
     * 是否有效0无效，1有效
     */
    private Integer status;
    /**
     * 是否是不可更改的 0 可更改 ，1不可更改
     */
    private Integer immutable;
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

    public StockInventory() {
    }

    public StockInventory(String inventoryOrderId, String inventoryName, Integer inventoryType, Integer factoryId,
                          Integer enterpriseId, String remarks, Integer status, Integer immutable, Integer createId,
                          String creator, Date createTime) {
        this.inventoryOrderId = inventoryOrderId;
        this.inventoryName = inventoryName;
        this.inventoryType = inventoryType;
        this.factoryId = factoryId;
        this.enterpriseId = enterpriseId;
        this.remarks = remarks;
        this.status = status;
        this.immutable = immutable;
        this.createId = createId;
        this.creator = creator;
        this.createTime = createTime;
    }
}
