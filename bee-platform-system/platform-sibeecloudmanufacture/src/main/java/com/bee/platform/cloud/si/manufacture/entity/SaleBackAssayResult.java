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
 * 销售车次化验结果反馈表
 * </p>
 *
 * @author chenxm66777123
 * @since 2019-09-27
 */
@Data
@EqualsAndHashCode(callSuper = true)
@Accessors(chain = true)
public class SaleBackAssayResult extends Model<SaleBackAssayResult> {

    private static final long serialVersionUID = 1L;

    /**
     * 主键
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Long id;
    /**
     * 承运方运输车次ID
     */
    private String carrierTransportDetailId;
    /**
     * 化验项
     */
    private String assayItem;
    /**
     * 化验结果
     */
    private Double assayValue;
    /**
     * 化验单位（0 %百分比  1 ‱万分比）
     */
    private Integer testUnit;
    /**
     * 化验单位名称
     */
    private String unitString;
    /**
     * 类型0输入项1输出项
     */
    private Integer type;
    /**
     * 状态0删除1启用
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
