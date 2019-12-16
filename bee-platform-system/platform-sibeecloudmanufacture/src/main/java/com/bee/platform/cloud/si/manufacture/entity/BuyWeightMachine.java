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

import io.swagger.models.auth.In;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.Accessors;

/**
 * <p>
 * 地磅数据信息
 * </p>
 *
 * @author chenxm66777123
 * @since 2019-09-26
 */
@Data
@EqualsAndHashCode(callSuper = true)
@Accessors(chain = true)
public class BuyWeightMachine extends Model<BuyWeightMachine> {

    private static final long serialVersionUID = 1L;

    /**
     * 自增id
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Long id;
    /**
     * 企业id
     */
    private Integer enterpriseId;
    /**
     * 工厂id
     */
    private Integer factoryId;
    /**
     * 磅单业务id
     */
    private String machineId;
    /**
     * 采购合同业务id
     */
    private String contractBusinessId;
    /**
     * 合同编号
     */
    private String contractNum;
    /**
     * 批次ID
     */
    private String batchId;
    /**
     * 车牌号
     */
    private String trainNumber;
    /**
     * 司机及联系方式
     */
    private String driver;
    /**
     * 设备id
     */
    private String deviceId;
    /**
     * 承运方
     */
    private String carrierName;
    /**
     * 发货单位
     */
    private String deliveryCompany;
    /**
     * 收货单位
     */
    private String receivingCompany;
    /**
     * 产品Id
     */
    private Integer productId;
    /**
     * 产品名称
     */
    private String productName;
    /**
     * 载货重量
     */
    private BigDecimal cargoWeight;
    /**
     * 称重时间
     */
    private Date weighingTime;
    /**
     * 进厂重量
     */
    private BigDecimal inFactoryWeight;
    /**
     * 出厂重量
     */
    private BigDecimal outFactoryWeight;
    /**
     * 进厂重量是否手动录入 0 自动 1手动
     */
    private Integer inFactoryWeightByManual;
    /**
     * 出厂重量是否手动录入 0 自动 1手动
     */
    private Integer outFactoryWeightByManual;
    /**
     * 进厂重量是否确认 0 未确认 1 确认
     */
    private Integer inFactoryWeightIsConfirm;
    /**
     * 进厂重量是否确认 0 未确认 1 确认
     */
    private Integer outFactoryWeightIsConfirm;
    /**
     * 净重
     */
    private BigDecimal netWeight;
    /**
     * 实际到厂时间
     */
    private Date arrivalTime;
    /**
     * 是否称重 0 未称重 1 已称重
     */
    private Integer isWeight;
    /**
     * 是否取样 0 未取样 1已取样
     */
    private Integer sampleStatus;
    /**
     * 取样推送状态 0未推送1已推送
     */
    private Integer samplePushStatus;
    /**
     * 取样推送时间
     */
    private Date samplePushTime;
    /**
     * 产品规格id
     */
    private Integer productSpecId;
    /**
     * 产品规格名称
     */
    private String productSpecName;
    /**
     * 同车次下样品的确认状态0-待确认1-已确认（当同车次样品确认后才是待确认）
     */
    private Integer qualityExamStatus;
    /**
     * 质检主任化验结果--0不合格 1合格
     */
    private Integer assayResult;
    /**
     * 折扣单价
     */
    private BigDecimal discountUnitPrice;
    /**
     * 数据状态0删除1正常
     */
    private Integer status;
    /**
     * 不合格车辆列表入库确认信息（0-待确认1-已确认)
     */
    private Integer inStorageConfirm;
    /**
     * 处理方式(处理方式（0-折价入库，1-确认入库）)
     */
    private Integer handleType;
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
    /**
     * 信息来源 0 物流推送 1 新增称重
     */
    private Integer dataSource;
    /**
     * 承运方id
     */
    private Integer carrierId;
    /**
     * 发货单位id
     */
    private Integer deliveryCompanyId;
    /**
     * 收货单位id
     */
    private Integer receivingCompanyId;
    /**
     * 扣重
     */
    private BigDecimal deductWeight;
    /**
     * 扣重显示按钮 0 确认扣重按钮 2 手动修改按钮
     */
    private Integer deductWeightByManual;
    /**
     * 司磅员
     */
    private String weightMan;
    /**
     * 0未结算 1已结算
     */
    private Integer settleStatus;
    /**
     * 结算时间
     */
    private Date settleTime;
    /**
     * 联系方式
     */
    private String contact;
    /**
     * 物流绑定是否忽略 0忽略 无效 1 不忽略 有效
     */
    private Integer bindIgnore;
    /**
     * 车次货物的扣重
     */
    private BigDecimal carDeductWeight;

    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
