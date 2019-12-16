package com.bee.platform.cloud.si.manufacture.entity;

import java.io.Serializable;

public class ZElecMeterData implements Serializable {
    /**
     * 主键id
     * 表字段 : z_elec_meter_data.id
     */
    private Integer id;

    /**
     * 工厂id
     * 表字段 : z_elec_meter_data.factory_id
     */
    private Integer factoryId;

    /**
     * 客户端id
     * 表字段 : z_elec_meter_data.api_client_id
     */
    private String apiClientId;

    /**
     * com端口
     * 表字段 : z_elec_meter_data.chanel_id
     */
    private String chanelId;

    /**
     * 电表id
     * 表字段 : z_elec_meter_data.device_id
     */
    private String deviceId;

    /**
     * 读表时间
     * 表字段 : z_elec_meter_data.meter_read_time
     */
    private Long meterReadTime;

    /**
     * 总有功电能
     * 表字段 : z_elec_meter_data.EP
     */
    private Float ep;

    /**
     * 总输入有功电能
     * 表字段 : z_elec_meter_data.EP_in
     */
    private Float epIn;

    /**
     * 总输出有功电能
     * 表字段 : z_elec_meter_data.EP_out
     */
    private Float epOut;

    /**
     * 总无功电能
     * 表字段 : z_elec_meter_data.EQ
     */
    private Float eq;

    /**
     * 总出入无功电能
     * 表字段 : z_elec_meter_data.EQ_in
     */
    private Float eqIn;

    /**
     * 总输出无功电能
     * 表字段 : z_elec_meter_data.EQ_out
     */
    private Float eqOut;

    /**
     * 频率
     * 表字段 : z_elec_meter_data.F
     */
    private Float f;

    /**
     * A相电流
     * 表字段 : z_elec_meter_data.Ia
     */
    private Float ia;

    /**
     * B相电流
     * 表字段 : z_elec_meter_data.Ib
     */
    private Float ib;

    /**
     * C相电流
     * 表字段 : z_elec_meter_data.Ic
     */
    private Float ic;

    /**
     * 总有功功率
     * 表字段 : z_elec_meter_data.P
     */
    private Float p;

    /**
     * A相有功功率
     * 表字段 : z_elec_meter_data.Pa
     */
    private Float pa;

    /**
     * B相有功功率
     * 表字段 : z_elec_meter_data.Pb
     */
    private Float pb;

    /**
     * C相有功功率
     * 表字段 : z_elec_meter_data.Pc
     */
    private Float pc;

    /**
     * 总功率因数
     * 表字段 : z_elec_meter_data.PF
     */
    private Float pf;

    /**
     * A相功率因数
     * 表字段 : z_elec_meter_data.PFa
     */
    private Float pfa;

    /**
     * B相功率因数
     * 表字段 : z_elec_meter_data.PFb
     */
    private Float pfb;

    /**
     * C相功率因数
     * 表字段 : z_elec_meter_data.PFc
     */
    private Float pfc;

    /**
     * 总无功功率
     * 表字段 : z_elec_meter_data.Q
     */
    private Float q;

    /**
     * A相无功功率
     * 表字段 : z_elec_meter_data.Qa
     */
    private Float qa;

    /**
     * B相无功功率
     * 表字段 : z_elec_meter_data.Qb
     */
    private Float qb;

    /**
     * C相无功功率
     * 表字段 : z_elec_meter_data.Qc
     */
    private Float qc;

    /**
     * 总视在功率
     * 表字段 : z_elec_meter_data.S
     */
    private Float s;

    /**
     * A相电压
     * 表字段 : z_elec_meter_data.Ua
     */
    private Float ua;

    /**
     * B相电压
     * 表字段 : z_elec_meter_data.Ub
     */
    private Float ub;

    /**
     * C相电压
     * 表字段 : z_elec_meter_data.Uc
     */
    private Float uc;

    /**
     * 总有功电度
     * 表字段 : z_elec_meter_data.Wp
     */
    private Float wp;

    /**
     * 0,未删除，1，已删除
     * 表字段 : z_elec_meter_data.logic_delete
     */
    private Integer logicDelete;

    private static final long serialVersionUID = 1L;

    public Integer getId() {
        return id;
    }

    public void setId(Integer id) {
        this.id = id;
    }

    public Integer getFactoryId() {
        return factoryId;
    }

    public void setFactoryId(Integer factoryId) {
        this.factoryId = factoryId;
    }

    public String getApiClientId() {
        return apiClientId;
    }

    public void setApiClientId(String apiClientId) {
        this.apiClientId = apiClientId == null ? null : apiClientId.trim();
    }

    public String getChanelId() {
        return chanelId;
    }

    public void setChanelId(String chanelId) {
        this.chanelId = chanelId == null ? null : chanelId.trim();
    }

    public String getDeviceId() {
        return deviceId;
    }

    public void setDeviceId(String deviceId) {
        this.deviceId = deviceId == null ? null : deviceId.trim();
    }

    public Long getMeterReadTime() {
        return meterReadTime;
    }

    public void setMeterReadTime(Long meterReadTime) {
        this.meterReadTime = meterReadTime;
    }

    public Float getEp() {
        return ep;
    }

    public void setEp(Float ep) {
        this.ep = ep;
    }

    public Float getEpIn() {
        return epIn;
    }

    public void setEpIn(Float epIn) {
        this.epIn = epIn;
    }

    public Float getEpOut() {
        return epOut;
    }

    public void setEpOut(Float epOut) {
        this.epOut = epOut;
    }

    public Float getEq() {
        return eq;
    }

    public void setEq(Float eq) {
        this.eq = eq;
    }

    public Float getEqIn() {
        return eqIn;
    }

    public void setEqIn(Float eqIn) {
        this.eqIn = eqIn;
    }

    public Float getEqOut() {
        return eqOut;
    }

    public void setEqOut(Float eqOut) {
        this.eqOut = eqOut;
    }

    public Float getF() {
        return f;
    }

    public void setF(Float f) {
        this.f = f;
    }

    public Float getIa() {
        return ia;
    }

    public void setIa(Float ia) {
        this.ia = ia;
    }

    public Float getIb() {
        return ib;
    }

    public void setIb(Float ib) {
        this.ib = ib;
    }

    public Float getIc() {
        return ic;
    }

    public void setIc(Float ic) {
        this.ic = ic;
    }

    public Float getP() {
        return p;
    }

    public void setP(Float p) {
        this.p = p;
    }

    public Float getPa() {
        return pa;
    }

    public void setPa(Float pa) {
        this.pa = pa;
    }

    public Float getPb() {
        return pb;
    }

    public void setPb(Float pb) {
        this.pb = pb;
    }

    public Float getPc() {
        return pc;
    }

    public void setPc(Float pc) {
        this.pc = pc;
    }

    public Float getPf() {
        return pf;
    }

    public void setPf(Float pf) {
        this.pf = pf;
    }

    public Float getPfa() {
        return pfa;
    }

    public void setPfa(Float pfa) {
        this.pfa = pfa;
    }

    public Float getPfb() {
        return pfb;
    }

    public void setPfb(Float pfb) {
        this.pfb = pfb;
    }

    public Float getPfc() {
        return pfc;
    }

    public void setPfc(Float pfc) {
        this.pfc = pfc;
    }

    public Float getQ() {
        return q;
    }

    public void setQ(Float q) {
        this.q = q;
    }

    public Float getQa() {
        return qa;
    }

    public void setQa(Float qa) {
        this.qa = qa;
    }

    public Float getQb() {
        return qb;
    }

    public void setQb(Float qb) {
        this.qb = qb;
    }

    public Float getQc() {
        return qc;
    }

    public void setQc(Float qc) {
        this.qc = qc;
    }

    public Float getS() {
        return s;
    }

    public void setS(Float s) {
        this.s = s;
    }

    public Float getUa() {
        return ua;
    }

    public void setUa(Float ua) {
        this.ua = ua;
    }

    public Float getUb() {
        return ub;
    }

    public void setUb(Float ub) {
        this.ub = ub;
    }

    public Float getUc() {
        return uc;
    }

    public void setUc(Float uc) {
        this.uc = uc;
    }

    public Float getWp() {
        return wp;
    }

    public void setWp(Float wp) {
        this.wp = wp;
    }

    public Integer getLogicDelete() {
        return logicDelete;
    }

    public void setLogicDelete(Integer logicDelete) {
        this.logicDelete = logicDelete;
    }

    @Override
    public boolean equals(Object that) {
        if (this == that) {
            return true;
        }
        if (that == null) {
            return false;
        }
        if (getClass() != that.getClass()) {
            return false;
        }
        ZElecMeterData other = (ZElecMeterData) that;
        return (this.getId() == null ? other.getId() == null : this.getId().equals(other.getId()))
            && (this.getFactoryId() == null ? other.getFactoryId() == null : this.getFactoryId().equals(other.getFactoryId()))
            && (this.getApiClientId() == null ? other.getApiClientId() == null : this.getApiClientId().equals(other.getApiClientId()))
            && (this.getChanelId() == null ? other.getChanelId() == null : this.getChanelId().equals(other.getChanelId()))
            && (this.getDeviceId() == null ? other.getDeviceId() == null : this.getDeviceId().equals(other.getDeviceId()))
            && (this.getMeterReadTime() == null ? other.getMeterReadTime() == null : this.getMeterReadTime().equals(other.getMeterReadTime()))
            && (this.getEp() == null ? other.getEp() == null : this.getEp().equals(other.getEp()))
            && (this.getEpIn() == null ? other.getEpIn() == null : this.getEpIn().equals(other.getEpIn()))
            && (this.getEpOut() == null ? other.getEpOut() == null : this.getEpOut().equals(other.getEpOut()))
            && (this.getEq() == null ? other.getEq() == null : this.getEq().equals(other.getEq()))
            && (this.getEqIn() == null ? other.getEqIn() == null : this.getEqIn().equals(other.getEqIn()))
            && (this.getEqOut() == null ? other.getEqOut() == null : this.getEqOut().equals(other.getEqOut()))
            && (this.getF() == null ? other.getF() == null : this.getF().equals(other.getF()))
            && (this.getIa() == null ? other.getIa() == null : this.getIa().equals(other.getIa()))
            && (this.getIb() == null ? other.getIb() == null : this.getIb().equals(other.getIb()))
            && (this.getIc() == null ? other.getIc() == null : this.getIc().equals(other.getIc()))
            && (this.getP() == null ? other.getP() == null : this.getP().equals(other.getP()))
            && (this.getPa() == null ? other.getPa() == null : this.getPa().equals(other.getPa()))
            && (this.getPb() == null ? other.getPb() == null : this.getPb().equals(other.getPb()))
            && (this.getPc() == null ? other.getPc() == null : this.getPc().equals(other.getPc()))
            && (this.getPf() == null ? other.getPf() == null : this.getPf().equals(other.getPf()))
            && (this.getPfa() == null ? other.getPfa() == null : this.getPfa().equals(other.getPfa()))
            && (this.getPfb() == null ? other.getPfb() == null : this.getPfb().equals(other.getPfb()))
            && (this.getPfc() == null ? other.getPfc() == null : this.getPfc().equals(other.getPfc()))
            && (this.getQ() == null ? other.getQ() == null : this.getQ().equals(other.getQ()))
            && (this.getQa() == null ? other.getQa() == null : this.getQa().equals(other.getQa()))
            && (this.getQb() == null ? other.getQb() == null : this.getQb().equals(other.getQb()))
            && (this.getQc() == null ? other.getQc() == null : this.getQc().equals(other.getQc()))
            && (this.getS() == null ? other.getS() == null : this.getS().equals(other.getS()))
            && (this.getUa() == null ? other.getUa() == null : this.getUa().equals(other.getUa()))
            && (this.getUb() == null ? other.getUb() == null : this.getUb().equals(other.getUb()))
            && (this.getUc() == null ? other.getUc() == null : this.getUc().equals(other.getUc()))
            && (this.getWp() == null ? other.getWp() == null : this.getWp().equals(other.getWp()))
            && (this.getLogicDelete() == null ? other.getLogicDelete() == null : this.getLogicDelete().equals(other.getLogicDelete()));
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((getId() == null) ? 0 : getId().hashCode());
        result = prime * result + ((getFactoryId() == null) ? 0 : getFactoryId().hashCode());
        result = prime * result + ((getApiClientId() == null) ? 0 : getApiClientId().hashCode());
        result = prime * result + ((getChanelId() == null) ? 0 : getChanelId().hashCode());
        result = prime * result + ((getDeviceId() == null) ? 0 : getDeviceId().hashCode());
        result = prime * result + ((getMeterReadTime() == null) ? 0 : getMeterReadTime().hashCode());
        result = prime * result + ((getEp() == null) ? 0 : getEp().hashCode());
        result = prime * result + ((getEpIn() == null) ? 0 : getEpIn().hashCode());
        result = prime * result + ((getEpOut() == null) ? 0 : getEpOut().hashCode());
        result = prime * result + ((getEq() == null) ? 0 : getEq().hashCode());
        result = prime * result + ((getEqIn() == null) ? 0 : getEqIn().hashCode());
        result = prime * result + ((getEqOut() == null) ? 0 : getEqOut().hashCode());
        result = prime * result + ((getF() == null) ? 0 : getF().hashCode());
        result = prime * result + ((getIa() == null) ? 0 : getIa().hashCode());
        result = prime * result + ((getIb() == null) ? 0 : getIb().hashCode());
        result = prime * result + ((getIc() == null) ? 0 : getIc().hashCode());
        result = prime * result + ((getP() == null) ? 0 : getP().hashCode());
        result = prime * result + ((getPa() == null) ? 0 : getPa().hashCode());
        result = prime * result + ((getPb() == null) ? 0 : getPb().hashCode());
        result = prime * result + ((getPc() == null) ? 0 : getPc().hashCode());
        result = prime * result + ((getPf() == null) ? 0 : getPf().hashCode());
        result = prime * result + ((getPfa() == null) ? 0 : getPfa().hashCode());
        result = prime * result + ((getPfb() == null) ? 0 : getPfb().hashCode());
        result = prime * result + ((getPfc() == null) ? 0 : getPfc().hashCode());
        result = prime * result + ((getQ() == null) ? 0 : getQ().hashCode());
        result = prime * result + ((getQa() == null) ? 0 : getQa().hashCode());
        result = prime * result + ((getQb() == null) ? 0 : getQb().hashCode());
        result = prime * result + ((getQc() == null) ? 0 : getQc().hashCode());
        result = prime * result + ((getS() == null) ? 0 : getS().hashCode());
        result = prime * result + ((getUa() == null) ? 0 : getUa().hashCode());
        result = prime * result + ((getUb() == null) ? 0 : getUb().hashCode());
        result = prime * result + ((getUc() == null) ? 0 : getUc().hashCode());
        result = prime * result + ((getWp() == null) ? 0 : getWp().hashCode());
        result = prime * result + ((getLogicDelete() == null) ? 0 : getLogicDelete().hashCode());
        return result;
    }

    @Override
	public String toString() {
		return "ZElecMeterData [id=" + id + ", factoryId=" + factoryId + ", apiClientId=" + apiClientId + ", chanelId="
				+ chanelId + ", deviceId=" + deviceId + ", meterReadTime=" + meterReadTime + ", ep=" + ep + ", epIn="
				+ epIn + ", epOut=" + epOut + ", eq=" + eq + ", eqIn=" + eqIn + ", eqOut=" + eqOut + ", f=" + f
				+ ", ia=" + ia + ", ib=" + ib + ", ic=" + ic + ", p=" + p + ", pa=" + pa + ", pb=" + pb + ", pc=" + pc
				+ ", pf=" + pf + ", pfa=" + pfa + ", pfb=" + pfb + ", pfc=" + pfc + ", q=" + q + ", qa=" + qa + ", qb="
				+ qb + ", qc=" + qc + ", s=" + s + ", ua=" + ua + ", ub=" + ub + ", uc=" + uc + ", wp=" + wp
				+ ", logicDelete=" + logicDelete + "]";
	}
    
    
}