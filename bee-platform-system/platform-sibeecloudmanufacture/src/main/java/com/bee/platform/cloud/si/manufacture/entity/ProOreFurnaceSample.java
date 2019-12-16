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
 *
 * </p>
 *
 * @author huangxin123
 * @since 2019-10-08
 */
@Data
@EqualsAndHashCode(callSuper = true)
@Accessors(chain = true)
public class ProOreFurnaceSample extends Model<ProOreFurnaceSample> {

    private static final long serialVersionUID = 1L;

    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 炉号id
     */
    private Integer furnaceId;
    /**
     * 班次:1一班，2二班，3三班
     */
    private Integer shift;
    /**
     * 矿热炉记录基本信息id
     */
    private Long oreRecordId;
    /**
     * 出炉批次:1一次，2二次，3三次
     */
    private Integer furnaceBatch;
    /**
     * 开班时间(矿热炉记录创建时间)
     */
    private Date openTime;
    /**
     * 创建时间
     */
    private Date createTime;
    /**
     * 更新时间
     */
    private Date modifyTime;
    /**
     * 创建人id
     */
    private Integer createId;
    /**
     * 修改人id
     */
    private Integer modifyId;
    /**
     * 工厂id
     */
    private Integer factoryId;
    /**
     * 企业id
     */
    private Integer companyId;
    /**
     * 是否取样 0 未取样 1已取样
     */
    private Integer sampleStatus;
    /**
     * 状态0删除 1正常
     */
    private Integer status;


    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
