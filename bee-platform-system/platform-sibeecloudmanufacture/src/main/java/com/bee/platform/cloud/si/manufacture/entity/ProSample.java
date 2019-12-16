package com.bee.platform.cloud.si.manufacture.entity;

import com.baomidou.mybatisplus.activerecord.Model;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.enums.IdType;
import lombok.Data;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

/**
 * <p>
 * 生产样品表
 * </p>
 *
 * @author liliang123
 * @since 2019-09-27
 */
@Data
@Accessors(chain = true)
public class ProSample extends Model<ProSample> {

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
     * 矿热炉记录表id
     */
    private Integer proOreFurnaceSampleId;
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
     * 炉号id
     */
    private Integer furnaceId;
    /**
     * 炉号名称
     */
    private String furnaceName;
    /**
     * 班次:1一班，2二班，3三班
     */
    private Integer shift;
    /**
     * 出炉批次
     */
    private Integer furnaceBatch;
    /**
     * 出炉日期
     */
    private String bakedDate;
    /**
     * 出炉时间
     */
    private String bakedTime;
    /**
     * 开班时间
     */
    private Date openTime;
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
