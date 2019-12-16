package com.bee.platform.cloud.si.manufacture.entity;

import java.io.Serializable;

import java.math.BigDecimal;
import java.util.Date;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.enums.IdType;
import com.baomidou.mybatisplus.activerecord.Model;
import java.io.Serializable;

import com.baomidou.mybatisplus.annotations.Version;

import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.Accessors;

/**
 * <p>
 * 物流批次表(采购)
 * </p>
 *
 * @author qhwang
 * @since 2019-09-23
 */
@Data
@EqualsAndHashCode(callSuper = true)
@Accessors(chain = true)
@TableName("buy_logistics_batch")
public class BuyLogisticsBatch extends Model<BuyLogisticsBatch> {

    private static final long serialVersionUID = 5305814713737930958L;

    /**
     * id
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Long id;
    /**
     * 批次ID
     */
    private String batchId;
    /**
     * 采购合同业务id
     */
    private String contractBusinessId;
    /**
     * 批次名称
     */
    private String batchName;
    /**
     * 运量
     */
    private BigDecimal freightVolume;
    /**
     * 到货量
     */
    private BigDecimal arrivalVolume;
    /**
     * 批次状态(1-待确认)
     */
    private Integer batchStatus;
    /**
     * 数据状态0删除1正常
     */
    private Integer status;
    /**
     * 创建人id
     */
    private Integer createId;
    /**
     * 创建人名称
     */
    private String creator;
    /**
     * 创建/申请时间
     */
    private Date createTime;
    /**
     * 修改人id
     */
    private Integer modifyId;
    /**
     * 修改人
     */
    private String modifier;
    /**
     * 修改时间
     */
    private Date modifyTime;
    /**
     * 备注
     */
    private String remark;

    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
