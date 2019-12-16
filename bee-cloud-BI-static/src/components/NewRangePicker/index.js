import { Component } from 'react';
import PropTypes from "prop-types";
import { DatePicker, message } from 'antd';
import styles from './index.less';
import moment from 'moment';

const { RangePicker } = DatePicker;
const targetFormat = 'YYYY-MM-DD';

export default class NewRangePicker extends Component {
  state = {
    timeType: 3, //0-日，1-7天，2-30天，3-12月
    startTime: moment().subtract('months', 12).format(targetFormat),
    endTime: moment().format(targetFormat),
  }

  componentDidMount() {
    const { onChange } = this.props;
    const { timeType, startTime, endTime } = this.state;
    onChange && onChange({ timeType, dateStrings: [startTime, endTime] })
  }

  changeTimeType = (timeType) => {
    const { onChange } = this.props;
    let startTime, endTime;
    if (timeType === 0) {
      startTime = moment().startOf('day').format(targetFormat)
      endTime = moment().format(targetFormat)
    } else if (timeType === 1) {
      startTime = moment().subtract('days', 7).format(targetFormat)
      endTime = moment().format(targetFormat)
    } else if (timeType === 2) {
      startTime = moment().subtract('days', 30).format(targetFormat)
      endTime = moment().format(targetFormat)
    } else {
      startTime = moment().subtract('months', 12).format(targetFormat)
      endTime = moment().format(targetFormat)
    }

    this.setState({
      timeType,
      startTime,
      endTime
    }, () => {
      onChange && onChange({ timeType, dateStrings: [startTime, endTime] })
    })
  }

  changeTime = (dateStrings) => {
    const { checkLength, onChange } = this.props;
    const { timeType } = this.state;
    if (checkLength) {
      if (timeType === 0 || timeType === 1 || timeType === 2) {
        if (moment(dateStrings[1]).diff(moment(dateStrings[0]), 'days') > 30) {
          return message.warning("时间差距大于30天，请重新选择日期");
        }
      } else {
        if (moment(dateStrings[1]).diff(moment(dateStrings[0]), 'months') > 12) {
          return message.warning("时间差距大于12个月，请重新选择日期");
        }
      }
    }
    this.setState({
      startTime: dateStrings[0],
      endTime: dateStrings[1]
    }, () => {
      onChange && onChange({ timeType, dateStrings: [dateStrings[0], dateStrings[1]] })
    })
  }

  render() {
    const { showOne } = this.props;
    const { timeType, startTime, endTime } = this.state;
    return (
      <div className={styles.buttonBox}>
        <div className={styles.btn}>
          {
            showOne ? <span className={timeType === 0 ? styles.span : ''} onClick={this.changeTimeType.bind(this, 0)}>1天</span> : null
          }
          <span className={timeType === 1 ? styles.span : ''} onClick={this.changeTimeType.bind(this, 1)}>7天</span>
          <span className={timeType === 2 ? styles.span : ''} onClick={this.changeTimeType.bind(this, 2)}>30天</span>
          <span className={timeType === 3 ? styles.span : ''} onClick={this.changeTimeType.bind(this, 3)}>12月</span>
        </div>
        <RangePicker
          value={[moment(startTime), moment(endTime)]}
          allowClear={false}
          format={targetFormat}
          showTime={false}
          onChange={(dates, dateStrings) => this.changeTime(dateStrings)}
          style={{ marginLeft: 24 }}
        />
      </div>
    )
  }
}

//限定控件传入的属性类型
NewRangePicker.propTypes = {
  showOne: PropTypes.bool, // 是否显示1天选项
  checkLength: PropTypes.bool, // 是否根据日期类型校验天数
  onChange: PropTypes.func, // 时间变化回调
};

//设置默认属性
NewRangePicker.defaultProps = {
  showOne: true, // 默认显示
  checkLength: false, //默认不校验
  onChange: () => false
};