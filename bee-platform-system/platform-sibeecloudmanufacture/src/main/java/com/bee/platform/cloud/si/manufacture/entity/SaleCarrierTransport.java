package com.bee.platform.cloud.si.manufacture.entity;

import java.io.Serializable;

import com.baomidou.mybatisplus.annotations.TableName;
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
 * 运输段承运方表(销售)
 * </p>
 *
 * @author qhwang
 * @since 2019-09-26
 */
@Data
@EqualsAndHashCode(callSuper = true)
@Accessors(chain = true)
@TableName("sale_carrier_transport")
public class SaleCarrierTransport extends Model<SaleCarrierTransport> {

    private static final long serialVersionUID = -6139903732398219765L;

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
     * 运输段id
     */
    private String transportSectionId;
    /**
     * 承运方运输段ID
     */
    private String carrierTransportId;
    /**
     * 承运方id
     */
    private Long carrierId;
    /**
     * 承运方
     */
    private String carrierName;
    /**
     * 运输方式(1-汽车 2-轮船 3-火车)
     */
    private Integer transportMode;
    /**
     * 是否到厂(0-不到厂 1-到厂)
     */
    private Integer toFactory;
    /**
     * 起始地地点id
     */
    private Integer startingPlaceId;
    /**
     * 起始地
     */
    private String startingPlace;
    /**
     * 到达地地点id
     */
    private Integer arrivalPlaceId;
    /**
     * 到达地
     */
    private String arrivalPlace;
    /**
     * 运量
     */
    private BigDecimal freightVolume;
    /**
     * 单价
     */
    private BigDecimal unitPrice;
    /**
     * 运费
     */
    private BigDecimal carriage;
    /**
     * 出发时间
     */
    private Date departureTime;
    /**
     * 预计到达时间
     */
    private Date estimateArrivalTime;
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
