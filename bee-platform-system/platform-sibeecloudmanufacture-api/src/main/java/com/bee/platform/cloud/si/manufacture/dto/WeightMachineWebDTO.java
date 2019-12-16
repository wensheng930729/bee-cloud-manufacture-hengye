package com.bee.platform.cloud.si.manufacture.dto;

import com.bee.platform.cloud.si.manufacture.rq.CommonFileRq;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;
import java.util.List;

/**
 * @author chenxm66777123
 * @version 1.0.0
 * @Description 地磅数据信息保存入参
 * @Date 2019/9/23 19:23
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "地磅数据信息保存出参")
public class WeightMachineWebDTO implements Serializable {

    @ApiModelProperty(value = "磅单业务id")
    private String machineId;

    @ApiModelProperty(value = "企业id")
    private Integer enterpriseId;

    @ApiModelProperty(value = "工厂id")
    private Integer factoryId;

    @ApiModelProperty(value = "采购合同业务id")
    private String contractBusinessId;

    @ApiModelProperty(value = "合同编号")
    private String contractNum;

    @ApiModelProperty(value = "批次ID")
    private String batchId;

    @ApiModelProperty(value = "车牌号")
    private String trainNumber;

    @ApiModelProperty(value = "司机及联系方式")
    private String driver;

    @ApiModelProperty(value = "设备id")
    private String deviceId;

    @ApiModelProperty(value = "承运方")
    private String carrierName;

    @ApiModelProperty(value = "发货单位")
    private String deliveryCompany;

    @ApiModelProperty(value = "收货单位")
    private String receivingCompany;

    @ApiModelProperty(value = "产品Id")
    private Integer productId;

    @ApiModelProperty(value = "产品名称")
    private String productName;

    @ApiModelProperty(value = "载货重量")
    private BigDecimal cargoWeight;

    @ApiModelProperty(value = "称重时间")
    private Date weighingTime;

    @ApiModelProperty(value = "进厂重量")
    private BigDecimal inFactoryWeight;

    @ApiModelProperty(value = "出厂重量")
    private BigDecimal outFactoryWeight;

    @ApiModelProperty(value = "进厂重量是否手动录入 0 自动 1手动 2 手动（设备异常）")
    private Integer inFactoryWeightByManual;

    @ApiModelProperty(value = "出厂重量是否手动录入 0 自动 1手动 2 手动（设备异常）")
    private Integer outFactoryWeightByManual;

    @ApiModelProperty(value = "进厂重量是否确认 0 未确认 1 确认")
    private Integer inFactoryWeightIsConfirm;

    @ApiModelProperty(value = "出厂重量是否确认 0 未确认 1 确认")
    private Integer outFactoryWeightIsConfirm;

    @ApiModelProperty(value = "净重")
    private BigDecimal netWeight;

    @ApiModelProperty(value = "实际到厂时间")
    private Date arrivalTime;

    @ApiModelProperty(value = "数据来源 0 物流推送 1 新增称重")
    private Integer dataSource;

    @ApiModelProperty(value = "是否通知取样 0未通知 1已经通知")
    private Integer samplePushStatus;

    @ApiModelProperty(value = "附件信息")
    private List<CommonFileRq> files;

    @ApiModelProperty("扣重")
    private BigDecimal deductWeight;

    @ApiModelProperty("扣重显示按钮 0 确认扣重按钮 1 手动修改按钮")
    private Integer deductWeightByManual;

    @ApiModelProperty(value = "备注")
    private String remark;

    @ApiModelProperty(value = "创建日期")
    private Date createTime;

    @ApiModelProperty(value = "联系方式")
    private String contact;
}
