import React, { Component } from 'react'
import style from "./index.less"
import {Icon} from "antd"

export default class EconomicNum extends Component {
  constructor() {
    super()
    this.state = {

    }
  }
  render() {
    //[phase,inflation, economic, risk] 传入参数分别为 阶段 膨胀指数 经济指数 敞口比例
    // String,  low middle high , low middle high , 百分比或分数(Number)
    const data = this.props.data || []
    const phase = data[0],
          inflation = data[1],
          economic = data[2],
          risk = data[3];
    let iColor, iText, eColor, eText = "";
    let content = ""
    switch(inflation) {
      case "low":
        iColor = "#f04864";
        iText = "低";
        break;
      case "middle":
        iColor = "#facc14";
        iText = "中";
        break;
      case "high":
        iColor = "#2fc25b";
        iText = "高";
        break;
    }
    switch(economic) {
      case "low":
        eColor = "#f04864";
        eText = "低";
        break;
      case "middle":
        eColor = "#facc14";
        eText = "中";
        break;
      case "high":
        eColor = "#2fc25b";
        eText = "高";
        break;
    }
    const rColor = risk <= 0.2 ? "#f04864" : risk <= 0.5 ? "#facc14" : "#2fc25b";
    if (data.length === 0) {
      content = (
        <div className={style.EconomicNumNo}>
        <Icon type="frown" />
          没有搜到任何数据
        </div>
      )
    } else {
      content = (
        <div className={style.EconomicNum}>
          <div className={style.header}>
            <p>宏观经济指数</p>
          </div>
          <div className={style.content}>
            <table>
              <thead>
                <tr>
                  <th>周期阶段</th>
                  <th>通货膨胀</th>
                  <th>经济增长</th>
                  <th>风险敞口</th>
                </tr>
              </thead>
              <tbody>
                <tr>
                  <td>{phase}</td>
                  <td style={{color: iColor}}>{iText}</td>
                  <td style={{color: eColor}}>{eText}</td>
                  <td style={{color: rColor}}>{risk}</td>
                </tr>
              </tbody>
            </table>
          </div>
        </div>
      )
    }
    return (
      content
    )
  }
}
