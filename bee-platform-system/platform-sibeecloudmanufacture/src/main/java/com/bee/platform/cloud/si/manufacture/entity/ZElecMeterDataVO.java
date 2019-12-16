package com.bee.platform.cloud.si.manufacture.entity;

import com.bee.platform.cloud.si.manufacture.entity.ZElecMeterData;
import java.io.Serializable;

/**
 * @Author zhanghu
 * @Description:
 * @Date: Create in 9:24 2018/11/8
 * @modified by:
 */
public class ZElecMeterDataVO extends ZElecMeterData implements Serializable {
    private String currentToken;
    /**
     * 查询条件开始时间
     */
    private Long startTime;
    /**
     * 结束时间
     */
    private Long endTime;
    /**
     * 时间段  1：按天查询 2:按周查询 3：按月查询
     */
    private Integer duration;

    /**
     * 页码
     */
    private Integer page;
    /**
     * 每页数据个数
     */
    private Integer limit;

    /**
     * 设备名称
     */
    private String deviceName;

    /**
     * 工厂名称
     */
    private String factoryName;

    public String getDeviceName() {
        return deviceName;
    }

    public void setDeviceName(String deviceName) {
        this.deviceName = deviceName;
    }

    public String getFactoryName() {
        return factoryName;
    }

    public void setFactoryName(String factoryName) {
        this.factoryName = factoryName;
    }

    public Integer getLimit() {
        return limit;
    }

    public void setLimit(Integer limit) {
        this.limit = limit;
    }

    public Integer getPage() {
        return page;
    }

    public void setPage(Integer page) {
        this.page = page;
    }

    public Integer getDuration() {
        return duration;
    }

    public void setDuration(Integer duration) {
        this.duration = duration;
    }

    public Long getStartTime() {
        return startTime;
    }

    public void setStartTime(Long startTime) {
        this.startTime = startTime;
    }

    public Long getEndTime() {
        return endTime;
    }

    public void setEndTime(Long endTime) {
        this.endTime = endTime;
    }

    public String getCurrentToken() {
        return currentToken;
    }

    public void setCurrentToken(String currentToken) {
        this.currentToken = currentToken;
    }

	@Override
	public String toString() {
		return "ZElecMeterDataVO [currentToken=" + currentToken + ", startTime=" + startTime + ", endTime=" + endTime
				+ ", duration=" + duration + ", page=" + page + ", limit=" + limit + ", deviceName=" + deviceName
				+ ", factoryName=" + factoryName + "]," + super.toString();
	}
    
    
}
