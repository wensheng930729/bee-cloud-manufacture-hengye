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
 * 合同收款信息
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
@TableName("sale_contract_collection")
public class SaleContractCollection extends Model<SaleContractCollection> {


    private static final long serialVersionUID = 6265112211333007242L;
    /**
     * id
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Long id;
    /**
     * 合同收款情况业务id
     */
    private String contractCollectionBusinessId;
    /**
     * 采购合同业务id
     */
    private String contractBusinessId;
    /**
     * 收款序号
     */
    private String serialNum;
    /**
     * 收款时间
     */
    private Date receiveTime;
    /**
     * 结款金额
     */
    private BigDecimal paymentAmount;
    /**
     * 备注
     */
    private String remark;
    /**
     * 0正常 1删除
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
