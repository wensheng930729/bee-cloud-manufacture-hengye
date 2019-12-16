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
 * 合同付款信息
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
@TableName("buy_contract_payment")
public class BuyContractPayment extends Model<BuyContractPayment> {

    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 合同付款情况业务id
     */
    private String contractPaymentBusinessId;
    /**
     * 采购合同业务id
     */
    private String contractBusinessId;
    /**
     * 付款序号
     */
    private String serialNum;
    /**
     * 付款时间
     */
    private Date payTime;
    /**
     * 付款金额
     */
    private BigDecimal payAmount;
    /**
     * 备注
     */
    private String remark;
    /**
     * 状态0删除 1正常
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


    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
