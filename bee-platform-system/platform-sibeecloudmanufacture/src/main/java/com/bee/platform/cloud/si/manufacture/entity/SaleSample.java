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
 * 销售取样表
 * </p>
 *
 * @author liliang123
 * @since 2019-09-26
 */
@Data
@EqualsAndHashCode(callSuper = true)
@Accessors(chain = true)
public class SaleSample extends Model<SaleSample> {

    private static final long serialVersionUID = 1L;

    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 企业id
     */
    private Integer enterpriseId;
    /**
     * 工厂id
     */
    private Integer factoryId;
    /**
     * 合同业务
     */
    private String contractBusinessId;
    /**
     * 合同号
     */
    private String contractNum;
    /**
     * 样品编号
     */
    private String sampleCode;
    /**
     * 产品id
     */
    private Integer productId;
    /**
     * 产品名称
     */
    private String productName;
    /**
     * 产品规格id
     */
    private Integer productSpecId;
    /**
     * 产品规格名称
     */
    private String productSpecName;
    /**
     * 化验状态--1待化验、2化验中、3已化验、0已弃用
     */
    private Integer assayStatus;
    /**
     * 化验人id
     */
    private Integer assayId;
    /**
     * 化验人
     */
    private String assayPerson;
    /**
     * 化验时间
     */
    private Date assayTime;
    /**
     * 弃用原因
     */
    private String abandonReason;
    /**
     * 弃样人id
     */
    private Integer abandonId;
    /**
     * 弃样人
     */
    private String abandonPerson;
    /**
     * 弃用时间
     */
    private Date abandonTime;
    /**
     * 状态0删除 1正常
     */
    private Integer status;
    /**
     * 创建人id
     */
    private Integer createId;
    /**
     * 创建人
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
