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

import lombok.*;
import lombok.experimental.Accessors;

/**
 * <p>
 * 销售合同信息表
 * </p>
 *
 * @author jie.zhang123
 * @since 2019-09-26
 */
@NoArgsConstructor
@Setter
@Getter
@Accessors(chain=true)
@ToString
@TableName("sale_contract_basic")
public class SaleContractBasic extends Model<SaleContractBasic> {


    private static final long serialVersionUID = -6088630978227971935L;
    /**
     * id
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Long id;
    /**
     * 销售合同业务id
     */
    private String contractBusinessId;
    /**
     * 合同编号
     */
    private String contractNum;
    /**
     * 企业id
     */
    private Integer enterpriseId;
    /**
     * 工厂id
     */
    private Integer factoryId;
    /**
     * 客户id
     */
    private Integer customerId;
    /**
     * 客户名称
     */
    private String customerName;
    /**
     * 签订日期
     */
    private Date signDate;
    /**
     * 交货日期
     */
    private Date deliveryDate;
    /**
     * 地点id
     */
    private Integer addressId;
    /**
     * 到达地
     */
    private String arrivalAddress;
    /**
     * 产品id
     */
    private Integer productId;
    /**
     * 产品名称
     */
    private String productName;
    /**
     * 产品类别
     */
    private Integer categoryId;

    /**
     * 产品类别
     */
    private String categoryName;
    /**
     * 产品单位
     */
    private String unitValue;
    /**
     * 单价
     */
    private BigDecimal unitPrice;
    /**
     * 合同数量
     */
    private BigDecimal quantity;
    /**
     * 合同金额=合同数量*单价
     */
    private BigDecimal amount;
    /**
     * 销售方式 0自提 1包运
     */
    private Integer saleMode;
    /**
     * 结算总金额
     */
    private BigDecimal amountSettlementTotal;
    /**
     * 收款总金额
     */
    private BigDecimal amountCollectionTotal;
    /**
     * 完成状态0否 1是
     */
    private Integer completed;
    /**
     * 联系人
     */
    private String linkMan;
    /**
     * 联系人号码
     */
    private String linkPhone;
    /**
     * 确认方 0我方确认 1客户确认
     */
    private Integer confirmPart;
    /**
     * 质量要求
     */
    private String qualityRequirement;
    /**
     * 在途量
     */
    private BigDecimal trafficVolume;
    /**
     * 已收货量
     */
    private BigDecimal receivedVolume;
    /**
     * 已发量
     */
    private BigDecimal issuedVolume;
    /**
     * 未发量
     */
    private BigDecimal undeliveredVolume;
    /**
     * 完成数量
     */
    private BigDecimal completedVolume;
    /**
     * 是否取样 0 未取样 1已取样
     */
    private Integer sampleStatus;
    /**
     * 货物重量结算状态 0未结算 1已结算
     */
    private Integer settleStatus;
    /**
     * 状态0删除 1正常
     */
    private Integer status;
    /**
     * 备注
     */
    private String remark;
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
     * 合同类型 0短期合同 1长期协议
     */
    private Integer type;

    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
