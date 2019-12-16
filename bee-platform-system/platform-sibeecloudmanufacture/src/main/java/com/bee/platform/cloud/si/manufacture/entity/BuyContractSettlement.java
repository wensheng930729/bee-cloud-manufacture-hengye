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

import lombok.*;
import lombok.experimental.Accessors;

/**
 * <p>
 * 
 * </p>
 *
 * @author jie.zhang123
 * @since 2019-09-23
 */
@NoArgsConstructor
@Setter
@Getter
@Accessors(chain=true)
@ToString
@TableName("buy_contract_settlement")
public class BuyContractSettlement extends Model<BuyContractSettlement> {

    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 合同结算表业务id
     */
    private String contractSettlementBusinessId;
    /**
     * 采购合同业务id
     */
    private String contractBusinessId;
    /**
     * 结算序号
     */
    private String serialNum;
    /**
     * 结算时间
     */
    private Date settleTime;
    /**
     * 结算单价
     */
    private BigDecimal unitPriceSettlement;
    /**
     * 结算金额
     */
    private BigDecimal amountSettlement;
    /**
     * 收货数量
     */
    private BigDecimal weightReceive;
    /**
     * 结算数量
     */
    private BigDecimal weightSettle;
    /**
     * 结算水分
     */
    private BigDecimal waterContentSettle;
    /**
     * 处理方式 0退货/1折价入库/2确认入库
     */
    private Integer treatmentMode;
    /**
     * 结算状态 0重量确认 1价格确认 2完成
     */
    private Integer settlementStatus;
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
     * 车数
     */
    private Integer carNum;
    /**
     * 车数
     */
    private String weightIds;


    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
