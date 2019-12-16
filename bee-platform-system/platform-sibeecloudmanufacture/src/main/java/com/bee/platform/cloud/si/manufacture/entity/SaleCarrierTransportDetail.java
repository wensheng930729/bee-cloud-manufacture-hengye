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
 * 承运方运输详情表(销售)
 * </p>
 *
 * @author qhwang
 * @since 2019-09-26
 */
@Data
@EqualsAndHashCode(callSuper = true)
@Accessors(chain = true)
@TableName("sale_carrier_transport_detail")
public class SaleCarrierTransportDetail extends Model<SaleCarrierTransportDetail> {

    private static final long serialVersionUID = 7217837319532192266L;

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
     * 承运方运输ID
     */
    private String carrierTransportId;
    /**
     * 承运方运输车次ID
     */
    private String carrierTransportDetailId;
    /**
     * 运输方式(1-汽车 2-轮船 3-火车)
     */
    private Integer transportMode;
    /**
     * 车牌号/船号/火车车次
     */
    private String trainNumber;
    /**
     * 司机/船长
     */
    private String driver;
    /**
     * 司机/船长联系方式
     */
    private String contact;
    /**
     * 载货重量
     */
    private BigDecimal cargoWeight;
    /**
     * 到厂状态(0-未到厂 1-已到厂)
     */
    private Integer arrivalStatus;
    /**
     * 到货确认化验结果--0不合格 1合格
     */
    private Integer assayResult;
    /**
     * 折扣单价
     */
    private BigDecimal discountUnitPrice;
    /**
     * 处理方式（0-折价入库，1-确认入库）
     */
    private Integer handleType;
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
